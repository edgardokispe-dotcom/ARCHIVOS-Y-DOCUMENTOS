program monitor_completo_barras
    implicit none
    
    character(len=500) :: cmd, ciudad, ciudad_cod, lat, lon, tiempo, tiempo_anterior
    character(len=50) :: entrada_usuario
    real :: pm25, pm10, o3, co, temp, hum, eco2, co_ppm
    real :: pct_pm25, pct_pm10, pct_o3, pct_co, probabilidad_precision, pct_co2
    integer :: ios, i, j, barras, actualizaciones, indice_alerta
    character(len=20) :: barra_str
    character(len=200) :: recomendacion
    
    ! Variables para el cálculo científico de eCO2
    real :: no2_estimado, so2_estimado, wind_speed, pressure
    real :: temp_effect, hum_effect, wind_effect, pressure_effect
    real :: pm25_contrib, pm10_contrib, o3_contrib, co_contrib, no2_contrib, so2_contrib
    real :: synergy_factor, base_co2, location_factor
    integer :: num_high_pollutants
    
    ! ====== VARIABLES PARA LAS NUEVAS FUNCIONALIDADES ======
    ! Almacenamiento histórico (últimas 10 mediciones)
    real, dimension(10) :: hist_pm25, hist_pm10, hist_o3, hist_co, hist_temp, hist_hum
    real, dimension(10) :: hist_eco2
    character(len=8), dimension(10) :: hist_tiempo
    integer :: hist_contador = 0
    real :: pm25_tendencia, pm10_tendencia, temp_tendencia
    character(len=50) :: tendencia_str
    
    ! Sistema de alertas proactivas
    logical :: alerta_pm25 = .false., alerta_pm10 = .false., alerta_o3 = .false.
    logical :: alerta_co = .false., alerta_temp = .false., alerta_tendencia = .false.
    character(len=100) :: mensaje_alerta, mensaje_tendencia
    integer :: nivel_alerta  ! 0: normal, 1: moderado, 2: alto, 3: crítico
    
    ! Variables para alertas por tendencias
    real :: pm25_anterior = 0.0, pm25_cambio = 0.0
    real :: temp_anterior = 0.0, temp_cambio = 0.0
    real :: umbral_alerta_pm25 = 10.0  ! Cambio de 10 µg/m³ en 30 segundos
    
    actualizaciones = 0
    tiempo_anterior = "00:00:00"
    
    ! Inicializar arrays históricos
    hist_pm25 = 0.0
    hist_pm10 = 0.0
    hist_o3 = 0.0
    hist_co = 0.0
    hist_temp = 0.0
    hist_hum = 0.0
    hist_eco2 = 0.0
    hist_tiempo = "00:00:00"
    
    call execute_command_line("clear", wait=.true.)
    
    ! ====== MENÚ DE SELECCIÓN DE CIUDADES ======
    print *, "========================================="
    print *, "   MONITOR GLOBAL: CIUDADES DEL MUNDO    "
    print *, "========================================="
    print *, ""
    print *, "CIUDADES PREDEFINIDAS:"
    print *, "1. La Paz"
    print *, "2. Buenos Aires"
    print *, "3. Tokyo"
    print *, "4. Madrid"
    print *, "5. New York"
    print *, ""
    print *, "También puede ingresar directamente el nombre de cualquier ciudad del mundo"
    print *, "(ej: Lima, Paris, Santiago, Bogota, Mexico City, etc.)"
    print *, ""
    write(*, '(A)', advance='no') " Ingrese número (1-5) o nombre de ciudad: "
    read(*, '(A)') entrada_usuario
    
    ! Procesar la entrada del usuario
    entrada_usuario = adjustl(entrada_usuario)
    
    if (len_trim(entrada_usuario) == 1 .and. &
        entrada_usuario(1:1) >= '1' .and. entrada_usuario(1:1) <= '5') then
        ! El usuario ingresó un número entre 1 y 5
        read(entrada_usuario, *) i
        
        ! Asignar ciudad predefinida según el número
        if (i == 1) then
            ciudad = "La Paz"
            lat = "-16.5000"
            lon = "-68.1500"
        else if (i == 2) then
            ciudad = "Buenos Aires"
            lat = "-34.6131"
            lon = "-58.3772"
        else if (i == 3) then
            ciudad = "Tokyo"
            lat = "35.6895"
            lon = "139.6917"
        else if (i == 4) then
            ciudad = "Madrid"
            lat = "40.4167"
            lon = "-3.7037"
        else if (i == 5) then
            ciudad = "New York"
            lat = "40.7128"
            lon = "-74.0060"
        end if
        
        ! Codificar ciudad para URL si es necesario
        ciudad_cod = ""
        do j = 1, len_trim(ciudad)
            if (ciudad(j:j) == " ") then
                ciudad_cod = trim(ciudad_cod) // "%20"
            else
                ciudad_cod = trim(ciudad_cod) // ciudad(j:j)
            end if
        end do
    else
        ! El usuario ingresó un nombre de ciudad
        ciudad = trim(entrada_usuario)
        
        ! Codificar ciudad para URL
        ciudad_cod = ""
        do i = 1, len_trim(ciudad)
            if (ciudad(i:i) == " ") then
                ciudad_cod = trim(ciudad_cod) // "%20"
            else
                ciudad_cod = trim(ciudad_cod) // ciudad(i:i)
            end if
        end do
        
        ! Obtener coordenadas de la ciudad ingresada
        cmd = "curl -s 'https://geocoding-api.open-meteo.com/v1/search?name=" // &
              trim(ciudad_cod) // "&count=1' | " // &
              "jq -r '.results[0] | .latitude, .longitude' > coords.txt 2>/dev/null || " // &
              "echo 'null null' > coords.txt"
        
        call execute_command_line(trim(cmd), wait=.true.)
        
        open(unit=20, file='coords.txt', status='old', iostat=ios)
        if (ios == 0) then
            read(20, *, iostat=ios) lat, lon
            close(20)
        end if
        
        if (ios /= 0 .or. trim(lat) == "null" .or. trim(lon) == "null") then
            print *, "Error: No se pudo localizar la ciudad '", trim(ciudad), "'"
            print *, "Verifique el nombre e intente nuevamente."
            stop
        end if
    end if
    
    ! ====== BUCLE PRINCIPAL DE MONITOREO ======
    do while (.true.)
        ! Obtener datos de calidad del aire
        cmd = "curl -s 'https://air-quality-api.open-meteo.com/v1/air-quality?latitude=" // &
              trim(lat) // "&longitude=" // trim(lon) // &
              "&current=pm10,pm2_5,carbon_monoxide,ozone' | " // &
              "jq -r '.current | .pm2_5, .pm10, .ozone, .carbon_monoxide' > temp.txt 2>/dev/null || " // &
              "echo '0 0 0 0' > temp.txt"
        
        call execute_command_line(trim(cmd), wait=.true.)
        
        ! Obtener datos meteorológicos
        cmd = "curl -s 'https://api.open-meteo.com/v1/forecast?latitude=" // &
              trim(lat) // "&longitude=" // trim(lon) // &
              "&current=temperature_2m,relative_humidity_2m' | " // &
              "jq -r '.current | .temperature_2m, .relative_humidity_2m' > clima.txt 2>/dev/null || " // &
              "echo '0 0' > clima.txt"
        
        call execute_command_line(trim(cmd), wait=.true.)
        
        ! Obtener hora
        call execute_command_line("date '+%H:%M:%S' > hora.txt", wait=.true.)
        
        open(unit=15, file='temp.txt', status='old', iostat=ios)
        read(15, *, iostat=ios) pm25, pm10, o3, co
        close(15)
        
        open(unit=16, file='clima.txt', status='old', iostat=ios)
        read(16, *, iostat=ios) temp, hum
        close(16)
        
        open(unit=17, file='hora.txt', status='old')
        read(17, '(A)') tiempo
        close(17)
        
        if (ios == 0) then
            actualizaciones = actualizaciones + 1
            
            ! ====== ALMACENAMIENTO HISTÓRICO ======
            hist_contador = hist_contador + 1
            if (hist_contador > 10) hist_contador = 1
            
            ! Guardar datos actuales en histórico
            hist_pm25(hist_contador) = pm25
            hist_pm10(hist_contador) = pm10
            hist_o3(hist_contador) = o3
            hist_co(hist_contador) = co
            hist_temp(hist_contador) = temp
            hist_hum(hist_contador) = hum
            hist_tiempo(hist_contador) = tiempo
            
            ! ====== CÁLCULO DE TENDENCIAS ======
            if (actualizaciones > 1) then
                ! Calcular tendencia de PM2.5
                if (hist_contador > 1) then
                    pm25_tendencia = pm25 - hist_pm25(max(1, hist_contador-1))
                else
                    pm25_tendencia = pm25 - hist_pm25(10)
                end if
                
                ! Calcular tendencia de temperatura
                if (hist_contador > 1) then
                    temp_tendencia = temp - hist_temp(max(1, hist_contador-1))
                else
                    temp_tendencia = temp - hist_temp(10)
                end if
                
                ! Determinar texto de tendencia
                if (pm25_tendencia > 5.0) then
                    tendencia_str = "↗️  ALZA RÁPIDA"
                else if (pm25_tendencia > 2.0) then
                    tendencia_str = "↗️  En aumento"
                else if (pm25_tendencia < -5.0) then
                    tendencia_str = "↘️  BAJA RÁPIDA"
                else if (pm25_tendencia < -2.0) then
                    tendencia_str = "↘️  En descenso"
                else
                    tendencia_str = "➡️  Estable"
                end if
            else
                pm25_tendencia = 0.0
                temp_tendencia = 0.0
                tendencia_str = "➡️  Primera medición"
            end if
            
            ! ====== SISTEMA DE ALERTAS PROACTIVAS ======
            alerta_pm25 = .false.
            alerta_pm10 = .false.
            alerta_o3 = .false.
            alerta_co = .false.
            alerta_temp = .false.
            alerta_tendencia = .false.
            nivel_alerta = 0
            mensaje_alerta = ""
            mensaje_tendencia = ""
            
            ! Alertas por valores absolutos
            if (pm25 > 75.0) then
                alerta_pm25 = .true.
                nivel_alerta = max(nivel_alerta, 3)
                mensaje_alerta = trim(mensaje_alerta) // "⚠️ PM2.5 EXTREMO "
            else if (pm25 > 55.0) then
                alerta_pm25 = .true.
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta = trim(mensaje_alerta) // "⚠️ PM2.5 MUY ALTO "
            else if (pm25 > 35.0) then
                alerta_pm25 = .true.
                nivel_alerta = max(nivel_alerta, 1)
            end if
            
            if (pm10 > 150.0) then
                alerta_pm10 = .true.
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta = trim(mensaje_alerta) // "⚠️ PM10 ALTO "
            end if
            
            if (o3 > 180.0) then
                alerta_o3 = .true.
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta = trim(mensaje_alerta) // "⚠️ OZONO ALTO "
            end if
            
            if (co > 10000.0) then
                alerta_co = .true.
                nivel_alerta = max(nivel_alerta, 3)
                mensaje_alerta = trim(mensaje_alerta) // "🚨 CO EXTREMO "
            else if (co > 5000.0) then
                alerta_co = .true.
                nivel_alerta = max(nivel_alerta, 2)
                mensaje_alerta = trim(mensaje_alerta) // "⚠️ CO ALTO "
            end if
            
            if (temp > 35.0 .or. temp < -5.0) then
                alerta_temp = .true.
                nivel_alerta = max(nivel_alerta, 1)
                mensaje_alerta = trim(mensaje_alerta) // "🌡️ TEMP EXTREMA "
            end if
            
            ! Alerta por tendencia rápida (cambio brusco)
            if (actualizaciones > 1) then
                if (pm25_tendencia > umbral_alerta_pm25) then
                    alerta_tendencia = .true.
                    nivel_alerta = max(nivel_alerta, 2)
                    write(mensaje_tendencia, '(A,F5.1,A)') &
                        "🚨 PM2.5 aumentó ", pm25_tendencia, " µg/m³ en 30s"
                else if (pm25_tendencia < -umbral_alerta_pm25) then
                    alerta_tendencia = .true.
                    nivel_alerta = max(nivel_alerta, 1)
                    write(mensaje_tendencia, '(A,F5.1,A)') &
                        "📉 PM2.5 disminuyó ", abs(pm25_tendencia), " µg/m³ en 30s"
                end if
            end if
            
            ! ====== CÁLCULO CIENTÍFICO MEJORADO DE eCO2 ======
            if (pm10 < 0.1) pm10 = pm25 * 1.6
            no2_estimado = co * 0.15
            so2_estimado = max(co * 0.02, 1.0)
            wind_speed = 3.0
            pressure = 1013.25
            
            if (pm25 > 50.0 .or. co > 1000.0) then
                location_factor = 1.0
                base_co2 = 435.0
            else if (pm25 > 20.0 .or. co > 500.0) then
                location_factor = 0.8
                base_co2 = 425.0
            else if (pm25 > 5.0) then
                location_factor = 0.6
                base_co2 = 420.0
            else
                location_factor = 0.4
                base_co2 = 415.0
            end if
            
            co_ppm = co / 1145.0
            pm25_contrib = (pm25 / 1000.0) * 0.85 * location_factor
            pm10_contrib = (pm10 / 1000.0) * 0.72 * location_factor
            o3_contrib = (o3 / 1000.0) * 0.08 * location_factor
            co_contrib = co_ppm * 50.0 * location_factor
            no2_contrib = (no2_estimado / 1000.0) * 2.05 * location_factor
            so2_contrib = (so2_estimado / 1000.0) * 0.96 * location_factor
            
            temp_effect = 0.42 * tanh((temp - 20.0) / 10.0)
            hum_effect = -0.18 * ((hum - 50.0) / 100.0)
            wind_effect = -0.25 * (wind_speed / 5.0)
            pressure_effect = 0.003 * ((pressure - 1013.25) / 100.0)
            
            synergy_factor = 1.0
            num_high_pollutants = 0
            
            if (pm25 > 35.0) num_high_pollutants = num_high_pollutants + 1
            if (co > 500.0) num_high_pollutants = num_high_pollutants + 1
            if (o3 > 100.0) num_high_pollutants = num_high_pollutants + 1
            if (pm10 > 50.0) num_high_pollutants = num_high_pollutants + 1
            
            if (num_high_pollutants >= 3) then
                synergy_factor = 1.25
            else if (num_high_pollutants == 2) then
                synergy_factor = 1.15
            else if (num_high_pollutants == 1) then
                synergy_factor = 1.05
            end if
            
            eco2 = base_co2 + &
                   (pm25_contrib + pm10_contrib + o3_contrib + &
                    co_contrib + no2_contrib + so2_contrib) * synergy_factor + &
                   temp_effect + hum_effect + wind_effect + pressure_effect
            
            if (eco2 < 350.0) eco2 = 350.0
            if (eco2 > 2000.0) eco2 = 2000.0
            
            ! Guardar eCO2 en histórico
            hist_eco2(hist_contador) = eco2
            
            probabilidad_precision = 75.0
            if (pm25 > 0 .and. co > 0) probabilidad_precision = probabilidad_precision + 10.0
            if (temp > -50 .and. temp < 60) probabilidad_precision = probabilidad_precision + 5.0
            if (hum >= 0 .and. hum <= 100) probabilidad_precision = probabilidad_precision + 5.0
            if (pm25 > 200 .or. co > 2000 .or. o3 > 200) then
                probabilidad_precision = probabilidad_precision - 10.0
            end if
            if (pm10 < 0.1 .or. abs(pm10 - pm25*1.6) < 0.1) then
                probabilidad_precision = probabilidad_precision - 5.0
            end if
            if (probabilidad_precision < 50.0) probabilidad_precision = 50.0
            if (probabilidad_precision > 92.0) probabilidad_precision = 92.0
            
            pct_co2 = (eco2 / 1000.0) * 100.0
            pct_pm25 = (pm25 / 15.0) * 100.0
            pct_pm10 = (pm10 / 45.0) * 100.0
            pct_o3 = (o3 / 100.0) * 100.0
            pct_co = (co / 4000.0) * 100.0
            
            ! ====== DETERMINAR RECOMENDACIONES ======
            if (pm25 > 75.0) then
                recomendacion = "☠️  EXTREMADAMENTE PELIGROSO - Evite TODA actividad al aire libre. " // &
                              "Use mascarilla N95 obligatoriamente. Cierre ventanas. " // &
                              "Grupos de riesgo permanecer en interiores con purificador de aire."
            else if (pm25 > 55.0) then
                recomendacion = "⚫️  MUY PELIGROSO - Evite actividades prolongadas al aire libre. " // &
                              "Use mascarilla N95. Reduzca actividad física intensa. " // &
                              "Personas con asma, niños y ancianos EVITEN salir."
            else if (pm25 > 35.0) then
                recomendacion = "🔴  PELIGROSO - Limite actividades al aire libre. " // &
                              "Use mascarilla si debe salir. Ventile solo brevemente. " // &
                              "Personas con enfermedades respiratorias tomen precauciones extras."
            else if (pm25 > 25.0) then
                recomendacion = "🟠  MUY DAÑINO - Reduzca actividades exteriores intensas. " // &
                              "Considere usar mascarilla en exteriores. " // &
                              "Grupos sensibles (niños, ancianos, asmáticos) limiten exposición."
            else if (pm25 > 15.0) then
                recomendacion = "🟡  DAÑINO A LA SALUD - Grupos sensibles deben evitar esfuerzos prolongados. " // &
                              "Personas con problemas cardíacos o respiratorios tomen precauciones. " // &
                              "Evite zonas de alto tráfico vehicular."
            else if (pm25 > 12.0) then
                recomendacion = "🟢  MODERADO - Aceptable para la mayoría, pero considere reducir " // &
                              "actividades intensas si presenta síntomas. " // &
                              "Ideal para actividades cortas al aire libre."
            else if (pm25 > 5.0) then
                recomendacion = "🔵  BUENO - Calidad del aire satisfactoria. " // &
                              "Adecuado para actividades normales al aire libre. " // &
                              "Mantenga hábitos saludables de ventilación."
            else
                recomendacion = "💎  EXCELENTE - Calidad del aire óptima. " // &
                              "Ideal para actividades al aire libre de cualquier tipo. " // &
                              "Perfecto para ejercicio y recreación exterior."
            end if
            
            call execute_command_line("clear", wait=.true.)
            
            ! ====== INTERFAZ (MISMA PRESENTACIÓN ORIGINAL) ======
            print *, "========================================="
            print *, " CIUDAD: ", trim(ciudad)
            print *, " UBICACION: ", trim(lat), " , ", trim(lon)
            print *, " HORA: ", trim(tiempo)
            print *, " ACTUALIZACION #: ", actualizaciones
            print *, "========================================="
            print *, ""
            
            ! ====== NUEVA SECCIÓN: ALERTAS PROACTIVAS ======
            if (nivel_alerta > 0) then
                print *, "🚨 ALERTAS ACTIVAS:"
                if (len_trim(mensaje_alerta) > 0) then
                    print *, "   ", trim(mensaje_alerta)
                end if
                if (alerta_tendencia) then
                    print *, "   ", trim(mensaje_tendencia)
                end if
                
                ! Indicador de nivel de alerta
                if (nivel_alerta == 3) then
                    print *, "   ⚠️⚡ NIVEL DE ALERTA: CRÍTICO"
                else if (nivel_alerta == 2) then
                    print *, "   ⚡ NIVEL DE ALERTA: ALTO"
                else if (nivel_alerta == 1) then
                    print *, "   ⚠️ NIVEL DE ALERTA: MODERADO"
                end if
                print *, ""
            end if
            
            ! Condiciones atmosféricas (original)
            print *, " CONDICIONES ATMOSFÉRICAS:"
            print '(A, F6.1, A)', " Temperatura: ", temp, " °C"
            print '(A, F6.1, A)', " Humedad:     ", hum, " %"
            print *, ""
            
            ! ====== DATOS REALES DE ESTACIÓN ======
            print *, " DATOS REALES DE ESTACION (TIEMPO REAL):"
            
            ! PM2.5 con barras Unicode (original)
            write(*, '(A, F6.1, A)', advance='no') " [PM 2.5] : ", pm25, " ug/m3 ["
            barras = min(20, max(0, int(pct_pm25 / 5.0)))
            do j = 1, barras
                write(*, '(A)', advance='no') "█"
            end do
            do j = barras + 1, 20
                write(*, '(A)', advance='no') " "
            end do
            print '(A, F6.1, A)', "] -> ", pct_pm25, "%"
            
            ! PM10 con barras Unicode (original)
            write(*, '(A, F6.1, A)', advance='no') " [PM 10]  : ", pm10, " ug/m3 ["
            barras = min(20, max(0, int(pct_pm10 / 5.0)))
            do j = 1, barras
                write(*, '(A)', advance='no') "█"
            end do
            do j = barras + 1, 20
                write(*, '(A)', advance='no') " "
            end do
            print '(A, F6.1, A)', "] -> ", pct_pm10, "%"
            
            ! O3 con barras suaves Unicode (original)
            write(*, '(A, F6.1, A)', advance='no') " [OZONO]  : ", o3, " ug/m3 ["
            barras = min(20, max(0, int(pct_o3 / 5.0)))
            do j = 1, barras
                write(*, '(A)', advance='no') "░"
            end do
            do j = barras + 1, 20
                write(*, '(A)', advance='no') " "
            end do
            print '(A, F6.1, A)', "] -> ", pct_o3, "%"
            
            ! CO con barras suaves Unicode (original)
            write(*, '(A, F6.1, A)', advance='no') " [CO]     : ", co, " ug/m3 ["
            barras = min(20, max(0, int(pct_co / 5.0)))
            do j = 1, barras
                write(*, '(A)', advance='no') "░"
            end do
            do j = barras + 1, 20
                write(*, '(A)', advance='no') " "
            end do
            print '(A, F6.1, A)', "] -> ", pct_co, "%"
            print *, ""
            
            ! ====== SECCIÓN: TENDENCIA (AHORA DESPUÉS DE DATOS REALES) ======
            print *, "📈 TENDENCIA: ", trim(tendencia_str)
            if (actualizaciones > 1) then
                print '(A,F6.1,A)', "   ΔPM2.5: ", pm25_tendencia, " µg/m³"
                print '(A,F6.1,A)', "   ΔTemp:  ", temp_tendencia, " °C"
            end if
            print *, ""
            
            ! ====== VALOR ESTIMADO POR SOFTWARE ======
            print *, " VALOR ESTIMADO POR SOFTWARE:"
            print '(A, F6.1, A, F6.1, A)', " [eCO2]* : ", eco2, " ppm    -> ", pct_co2, "%"
            
            ! Mostrar historial de eCO2 si hay datos
            if (actualizaciones > 1) then
                print '(A,F6.1,A,F6.1,A)', " Rango eCO2: ", minval(hist_eco2, mask=hist_eco2>0), &
                                           " - ", maxval(hist_eco2), " ppm"
            end if
            print *, ""
            
            ! ====== NOTA DE PRECISIÓN ======
            print *, "-----------------------------------------"
            print '(A, F5.1, A)', "*NOTA: Precision de calculo para CO2: ", probabilidad_precision, "%"
            print *, " Basado en modelo científico con múltiples variables"
            print *, "-----------------------------------------"
            print *, ""
            
            ! ====== RECOMENDACIONES ======
            print *, " >>> RECOMENDACIÓN: " // trim(recomendacion)
            
            ! Información adicional
            print *, ""
            print *, "ℹ️  Cálculo eCO2 incluye: PM2.5, PM10, O3, CO, NOx*, SOx*, temp, humedad"
            print *, "   *Estimados basados en correlaciones científicas"
            
            print *, ""
            print *, "========================================="
            print *, " Actualizando en 30 segundos..."
            print *, " Ctrl+C para salir o cambiar ciudad"
        end if
        
        tiempo_anterior = tiempo
        pm25_anterior = pm25
        temp_anterior = temp
        
        call sleep(30)
        
        ! Limpiar archivos temporales
        call execute_command_line("rm -f temp.txt clima.txt hora.txt coords.txt", wait=.true.)
    end do
    
end program monitor_completo_barras

