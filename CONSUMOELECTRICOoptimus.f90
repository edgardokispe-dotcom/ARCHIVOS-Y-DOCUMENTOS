PROGRAM consumoelectrico_completo
IMPLICIT NONE

! ======================================================
! DECLARACIÓN DE VARIABLES Y CONSTANTES
! ======================================================
REAL :: voltaje, corriente, potencia, horas_dia, dias
REAL :: consumo_kwh, precio_kwh, costo_total, factor_potencia
REAL :: precio_dia, precio_noche, co2, potencia_aparente, potencia_activa
REAL :: horas_dia_trabajo, horas_noche
REAL :: consumo_kwh_dia, consumo_kwh_noche, consumo_kwh_total
REAL :: costo_dia, costo_noche, factor_co2_personalizado
INTEGER :: opcion, tipo_carga, opcion_tarifa, continuar
CHARACTER(LEN=50) :: nombre_dispositivo
LOGICAL :: guardar_historial_flag  ! Cambiado el nombre

! Constantes (ahora variables para poder modificarlas)
REAL :: FACTOR_CO2 = 0.5          ! kg CO2 por kWh
REAL :: TARIFA1_DIA = 0.80        ! S/kWh normal
REAL :: TARIFA1_NOCHE = 0.60      ! S/kWh reducido
REAL :: TARIFA2_DIA = 0.95        ! S/kWh comercial
REAL :: TARIFA2_NOCHE = 0.75      ! S/kWh comercial noche

! ======================================================
! CONFIGURACIÓN INICIAL
! ======================================================
guardar_historial_flag = .FALSE.
factor_co2_personalizado = 0.5

! ======================================================
! BUCLE PRINCIPAL DEL PROGRAMA
! ======================================================
DO
    
    CALL LIMPIAR_PANTALLA()
    
    PRINT *, '============================================'
    PRINT *, '   CALCULADORA AVANZADA DE CONSUMO ELÉCTRICO'
    PRINT *, '============================================'
    PRINT *, ''
    
    ! ======================================================
    ! MENÚ DE OPCIONES
    ! ======================================================
    PRINT *, '--------------------------------------------'
    PRINT *, '         MENÚ PRINCIPAL'
    PRINT *, '--------------------------------------------'
    PRINT *, '1. Cálculo básico (DC/resistivo)'
    PRINT *, '2. Cálculo AC con factor de potencia'
    PRINT *, '3. Cálculo con tarifas diferenciadas'
    PRINT *, '4. Cálculo con emisiones CO2'
    PRINT *, '5. Ver historial de cálculos'
    PRINT *, '6. Configuración'
    PRINT *, '0. Salir'
    PRINT *, '--------------------------------------------'
    PRINT *, ''
    PRINT *, 'Seleccione opción:'
    READ *, opcion
    
    IF (opcion == 0) EXIT
    
    ! ======================================================
    ! OPCIÓN 1: CÁLCULO BÁSICO
    ! ======================================================
    IF (opcion == 1) THEN
        
        PRINT *, ''
        PRINT *, '--------------------------------------------'
        PRINT *, '         CÁLCULO BÁSICO (DC/RESISTIVO)'
        PRINT *, '--------------------------------------------'
        PRINT *, ''
        
        ! Entrada de datos
        PRINT *, 'Nombre del dispositivo:'
        READ *, nombre_dispositivo
        
        CALL LEER_VALIDADO('Voltaje (V): ', voltaje, 0.0, 1000.0)
        CALL LEER_VALIDADO('Corriente (A): ', corriente, 0.0, 100.0)
        CALL LEER_VALIDADO('Horas/día: ', horas_dia, 0.0, 24.0)
        CALL LEER_VALIDADO('Días/mes: ', dias, 0.0, 365.0)
        CALL LEER_VALIDADO('Precio kWh (S/): ', precio_kwh, 0.0, 10.0)
        
        ! Cálculos
        potencia = voltaje * corriente
        consumo_kwh = (potencia * horas_dia * dias) / 1000.0
        costo_total = consumo_kwh * precio_kwh
        
        ! Mostrar resultados
        CALL MOSTRAR_RESULTADOS(potencia, consumo_kwh, costo_total, &
                               nombre_dispositivo, voltaje, corriente, &
                               horas_dia, dias, precio_kwh)
        
        ! Guardar en historial si está activado
        IF (guardar_historial_flag) THEN
            CALL GUARDAR_HISTORIAL_FILE(nombre_dispositivo, consumo_kwh, costo_total)
        END IF
        
    ! ======================================================
    ! OPCIÓN 2: CÁLCULO AC CON FACTOR DE POTENCIA
    ! ======================================================
    ELSE IF (opcion == 2) THEN
        
        PRINT *, ''
        PRINT *, '--------------------------------------------'
        PRINT *, '   CÁLCULO AC CON FACTOR DE POTENCIA'
        PRINT *, '--------------------------------------------'
        PRINT *, ''
        
        PRINT *, 'Seleccione tipo de carga:'
        PRINT *, '1. Resistiva (lámparas, termas) - FP = 1.0'
        PRINT *, '2. Motores pequeños - FP = 0.8'
        PRINT *, '3. Motores grandes - FP = 0.85'
        PRINT *, '4. Equipos electrónicos - FP = 0.7'
        PRINT *, '5. Especificar factor manualmente'
        READ *, tipo_carga
        
        SELECT CASE (tipo_carga)
            CASE (1)
                factor_potencia = 1.0
            CASE (2)
                factor_potencia = 0.8
            CASE (3)
                factor_potencia = 0.85
            CASE (4)
                factor_potencia = 0.7
            CASE (5)
                CALL LEER_VALIDADO('Factor de potencia (0.1 a 1.0): ', &
                                   factor_potencia, 0.1, 1.0)
            CASE DEFAULT
                factor_potencia = 0.9
        END SELECT
        
        ! Entrada de datos
        PRINT *, ''
        PRINT *, 'Nombre del dispositivo:'
        READ *, nombre_dispositivo
        
        CALL LEER_VALIDADO('Voltaje (V): ', voltaje, 0.0, 1000.0)
        CALL LEER_VALIDADO('Corriente (A): ', corriente, 0.0, 100.0)
        CALL LEER_VALIDADO('Horas/día: ', horas_dia, 0.0, 24.0)
        CALL LEER_VALIDADO('Días/mes: ', dias, 0.0, 365.0)
        CALL LEER_VALIDADO('Precio kWh (S/): ', precio_kwh, 0.0, 10.0)
        
        ! Cálculos con factor de potencia
        potencia_aparente = voltaje * corriente
        potencia_activa = potencia_aparente * factor_potencia
        consumo_kwh = (potencia_activa * horas_dia * dias) / 1000.0
        costo_total = consumo_kwh * precio_kwh
        
        ! Mostrar resultados avanzados
        CALL MOSTRAR_RESULTADOS_AVANZADOS(potencia_aparente, potencia_activa, &
                                         consumo_kwh, costo_total, &
                                         nombre_dispositivo, factor_potencia, &
                                         voltaje, corriente, horas_dia, &
                                         dias, precio_kwh)
        
        ! Guardar en historial si está activado
        IF (guardar_historial_flag) THEN
            CALL GUARDAR_HISTORIAL_FILE(nombre_dispositivo, consumo_kwh, costo_total)
        END IF
        
    ! ======================================================
    ! OPCIÓN 3: TARIFAS DIFERENCIADAS
    ! ======================================================
    ELSE IF (opcion == 3) THEN
        
        PRINT *, ''
        PRINT *, '--------------------------------------------'
        PRINT *, '     CÁLCULO CON TARIFAS DIFERENCIADAS'
        PRINT *, '--------------------------------------------'
        PRINT *, ''
        
        PRINT *, 'Seleccione tipo de tarifa:'
        PRINT *, '1. Residencial'
        PRINT *, '2. Comercial'
        PRINT *, '3. Industrial'
        READ *, opcion_tarifa
        
        ! Asignar tarifas según tipo
        SELECT CASE (opcion_tarifa)
            CASE (1)
                precio_dia = TARIFA1_DIA
                precio_noche = TARIFA1_NOCHE
                PRINT *, 'Tarifa residencial seleccionada'
            CASE (2)
                precio_dia = TARIFA2_DIA
                precio_noche = TARIFA2_NOCHE
                PRINT *, 'Tarifa comercial seleccionada'
            CASE (3)
                CALL LEER_VALIDADO('Tarifa horario día (S/kWh): ', &
                                   precio_dia, 0.0, 10.0)
                CALL LEER_VALIDADO('Tarifa horario noche (S/kWh): ', &
                                   precio_noche, 0.0, 10.0)
            CASE DEFAULT
                precio_dia = 0.80
                precio_noche = 0.60
        END SELECT
        
        PRINT *, ''
        PRINT *, 'Nombre del dispositivo:'
        READ *, nombre_dispositivo
        
        CALL LEER_VALIDADO('Voltaje (V): ', voltaje, 0.0, 1000.0)
        CALL LEER_VALIDADO('Corriente (A): ', corriente, 0.0, 100.0)
        
        ! Separar horas día/noche
        PRINT *, 'Nota: El día es de 6:00 a 22:00 (16 horas)'
        PRINT *, '      La noche es de 22:00 a 6:00 (8 horas)'
        PRINT *, ''
        
        CALL LEER_VALIDADO('Horas de uso en horario DÍA (6am-10pm): ', &
                          horas_dia_trabajo, 0.0, 16.0)
        CALL LEER_VALIDADO('Horas de uso en horario NOCHE (10pm-6am): ', &
                          horas_noche, 0.0, 8.0)
        
        CALL LEER_VALIDADO('Días/mes: ', dias, 0.0, 365.0)
        
        ! Cálculos con tarifas diferenciadas
        potencia = voltaje * corriente
        
        ! Consumo por horario
        consumo_kwh_dia = (potencia * horas_dia_trabajo * dias) / 1000.0
        consumo_kwh_noche = (potencia * horas_noche * dias) / 1000.0
        consumo_kwh_total = consumo_kwh_dia + consumo_kwh_noche
        
        ! Costos por horario
        costo_dia = consumo_kwh_dia * precio_dia
        costo_noche = consumo_kwh_noche * precio_noche
        costo_total = costo_dia + costo_noche
        
        ! Mostrar resultados de tarifas
        CALL MOSTRAR_RESULTADOS_TARIFAS(potencia, consumo_kwh_total, costo_total, &
                                       nombre_dispositivo, voltaje, corriente, &
                                       horas_dia_trabajo, horas_noche, dias, &
                                       precio_dia, precio_noche, consumo_kwh_dia, &
                                       consumo_kwh_noche, costo_dia, costo_noche)
        
        ! Guardar en historial si está activado
        IF (guardar_historial_flag) THEN
            CALL GUARDAR_HISTORIAL_FILE(nombre_dispositivo, consumo_kwh_total, costo_total)
        END IF
        
    ! ======================================================
    ! OPCIÓN 4: CÁLCULO CON EMISIONES DE CO2
    ! ======================================================
    ELSE IF (opcion == 4) THEN
        
        PRINT *, ''
        PRINT *, '--------------------------------------------'
        PRINT *, '     CÁLCULO CON HUELLA DE CARBONO'
        PRINT *, '--------------------------------------------'
        PRINT *, ''
        
        PRINT *, 'Nombre del dispositivo:'
        READ *, nombre_dispositivo
        
        CALL LEER_VALIDADO('Voltaje (V): ', voltaje, 0.0, 1000.0)
        CALL LEER_VALIDADO('Corriente (A): ', corriente, 0.0, 100.0)
        CALL LEER_VALIDADO('Horas/día: ', horas_dia, 0.0, 24.0)
        CALL LEER_VALIDADO('Días/año: ', dias, 0.0, 365.0)
        CALL LEER_VALIDADO('Precio kWh (S/): ', precio_kwh, 0.0, 10.0)
        
        ! Cálculos básicos
        potencia = voltaje * corriente
        consumo_kwh = (potencia * horas_dia * dias) / 1000.0
        costo_total = consumo_kwh * precio_kwh
        
        ! Cálculo de emisiones de CO2 usando factor personalizado
        co2 = consumo_kwh * factor_co2_personalizado
        
        ! Mostrar resultados con huella de carbono
        CALL MOSTRAR_RESULTADOS_CO2(potencia, consumo_kwh, costo_total, co2, &
                                   nombre_dispositivo, voltaje, corriente, &
                                   horas_dia, dias, precio_kwh)
        
        ! Guardar en historial si está activado
        IF (guardar_historial_flag) THEN
            CALL GUARDAR_HISTORIAL_FILE(nombre_dispositivo, consumo_kwh, costo_total)
        END IF
        
    ! ======================================================
    ! OPCIÓN 5: VER HISTORIAL
    ! ======================================================
    ELSE IF (opcion == 5) THEN
        
        CALL MOSTRAR_HISTORIAL_FILE()
        
    ! ======================================================
    ! OPCIÓN 6: CONFIGURACIÓN
    ! ======================================================
    ELSE IF (opcion == 6) THEN
        
        PRINT *, ''
        PRINT *, '--------------------------------------------'
        PRINT *, '           CONFIGURACIÓN'
        PRINT *, '--------------------------------------------'
        PRINT *, ''
        
        PRINT *, '1. Activar/desactivar guardado de historial'
        PRINT *, '2. Establecer factor CO2 personalizado'
        PRINT *, '3. Configurar tarifas personalizadas'
        PRINT *, '4. Volver al menú principal'
        PRINT *, ''
        PRINT *, 'Seleccione opción:'
        READ *, opcion
        
        SELECT CASE (opcion)
            CASE (1)
                guardar_historial_flag = .NOT. guardar_historial_flag
                IF (guardar_historial_flag) THEN
                    PRINT *, 'HISTORIAL ACTIVADO'
                ELSE
                    PRINT *, 'HISTORIAL DESACTIVADO'
                END IF
            CASE (2)
                PRINT *, 'Nuevo factor CO2 (kg/kWh):'
                READ *, factor_co2_personalizado
                PRINT *, 'Factor CO2 actualizado a:', factor_co2_personalizado, 'kg/kWh'
            CASE (3)
                CALL CONFIGURAR_TARIFAS_PERS()
        END SELECT
        
    END IF
    
    ! ======================================================
    ! PAUSA Y CONTINUACIÓN
    ! ======================================================
    PRINT *, ''
    PRINT *, '--------------------------------------------'
    PRINT *, 'Presione 1 para continuar, 0 para salir'
    PRINT *, '--------------------------------------------'
    READ *, continuar
    
    IF (continuar == 0) EXIT
    
END DO

! ======================================================
! FIN DEL PROGRAMA
! ======================================================
CALL LIMPIAR_PANTALLA()
PRINT *, '============================================'
PRINT *, '      PROGRAMA FINALIZADO'
PRINT *, '      ¡GRACIAS POR SU USO!'
PRINT *, '============================================'
PRINT *, ''

CONTAINS

! ======================================================
! SUBRUTINAS AUXILIARES (INTERNAS AL PROGRAMA)
! ======================================================

SUBROUTINE LEER_VALIDADO(mensaje, valor, min_val, max_val)
    CHARACTER(LEN=*), INTENT(IN) :: mensaje
    REAL, INTENT(OUT) :: valor
    REAL, INTENT(IN) :: min_val, max_val
    
    DO
        PRINT *, mensaje
        READ *, valor
        IF (valor >= min_val .AND. valor <= max_val) EXIT
        PRINT *, 'Error: Valor fuera de rango (', min_val, '-', max_val, ')'
    END DO
END SUBROUTINE LEER_VALIDADO

SUBROUTINE LIMPIAR_PANTALLA()
    ! Simulación de limpiar pantalla
    INTEGER :: i
    DO i = 1, 30
        PRINT *, ''
    END DO
END SUBROUTINE LIMPIAR_PANTALLA

SUBROUTINE MOSTRAR_RESULTADOS(pot, cons, costo, nombre, v, i, h, d, precio)
    REAL, INTENT(IN) :: pot, cons, costo, v, i, h, d, precio
    CHARACTER(LEN=*), INTENT(IN) :: nombre
    
    PRINT *, ''
    PRINT *, '============================================'
    PRINT *, '          RESULTADOS DEL CÁLCULO'
    PRINT *, '============================================'
    PRINT *, ''
    PRINT *, 'Dispositivo: ', TRIM(nombre)
    PRINT *, '--------------------------------------------'
    PRINT 100, 'Potencia calculada      :', pot, 'W'
    PRINT 100, 'Consumo eléctrico       :', cons, 'kWh/mes'
    PRINT 100, 'Costo mensual estimado  : S/', costo
    PRINT *, '--------------------------------------------'
    PRINT *, 'Detalles:'
    PRINT 200, '  Voltaje                :', v, 'V'
    PRINT 200, '  Corriente              :', i, 'A'
    PRINT 200, '  Horas/día              :', h, 'h'
    PRINT 200, '  Días/mes               :', d, 'días'
    PRINT 200, '  Precio kWh             : S/', precio
    PRINT *, ''
    
100 FORMAT(1X, A, F12.2, 1X, A)
200 FORMAT(1X, A, F12.2, 1X, A)
END SUBROUTINE MOSTRAR_RESULTADOS

SUBROUTINE MOSTRAR_RESULTADOS_AVANZADOS(pot_ap, pot_ac, cons, costo, &
                                       nombre, fp, v, i, h, d, precio)
    REAL, INTENT(IN) :: pot_ap, pot_ac, cons, costo, fp, v, i, h, d, precio
    CHARACTER(LEN=*), INTENT(IN) :: nombre
    
    PRINT *, ''
    PRINT *, '============================================'
    PRINT *, '     RESULTADOS (CON FACTOR POTENCIA)'
    PRINT *, '============================================'
    PRINT *, ''
    PRINT *, 'Dispositivo: ', TRIM(nombre)
    PRINT *, 'Factor de potencia: ', fp
    PRINT *, '--------------------------------------------'
    PRINT 100, 'Potencia aparente (S)   :', pot_ap, 'VA'
    PRINT 100, 'Potencia activa (P)     :', pot_ac, 'W'
    PRINT 100, 'Consumo eléctrico       :', cons, 'kWh/mes'
    PRINT 100, 'Costo mensual estimado  : S/', costo
    PRINT *, '--------------------------------------------'
    PRINT *, 'Eficiencia eléctrica:'
    IF (fp >= 0.9) THEN
        PRINT *, 'Excelente (FP >= 0.9)'
    ELSE IF (fp >= 0.8) THEN
        PRINT *, 'Aceptable (FP 0.8-0.89)'
    ELSE
        PRINT *, 'Mejorable (FP < 0.8)'
    END IF
    PRINT *, ''
    
100 FORMAT(1X, A, F12.2, 1X, A)
END SUBROUTINE MOSTRAR_RESULTADOS_AVANZADOS

SUBROUTINE MOSTRAR_RESULTADOS_TARIFAS(pot, cons, costo, nombre, v, i, &
                                     h_dia, h_noche, d, precio_d, precio_n, &
                                     cons_d, cons_n, costo_d, costo_n)
    REAL, INTENT(IN) :: pot, cons, costo, v, i, h_dia, h_noche, d
    REAL, INTENT(IN) :: precio_d, precio_n, cons_d, cons_n, costo_d, costo_n
    CHARACTER(LEN=*), INTENT(IN) :: nombre
    
    PRINT *, ''
    PRINT *, '============================================'
    PRINT *, '  RESULTADOS (TARIFAS DIFERENCIADAS)'
    PRINT *, '============================================'
    PRINT *, ''
    PRINT *, 'Dispositivo: ', TRIM(nombre)
    PRINT *, '--------------------------------------------'
    PRINT 100, 'Potencia calculada      :', pot, 'W'
    PRINT 100, 'Consumo total           :', cons, 'kWh/mes'
    PRINT 100, 'Costo total mensual     : S/', costo
    PRINT *, ''
    PRINT *, 'DESGLOSE POR HORARIO:'
    PRINT *, '--------------------------------------------'
    PRINT *, 'Horario DÍA (6am-10pm):'
    PRINT 300, '  Horas/día  :', h_dia
    PRINT 300, '  Consumo    :', cons_d, 'kWh'
    PRINT 300, '  Tarifa     : S/', precio_d
    PRINT 300, '  Costo      : S/', costo_d
    PRINT *, ''
    PRINT *, 'Horario NOCHE (10pm-6am):'
    PRINT 300, '  Horas/día  :', h_noche
    PRINT 300, '  Consumo    :', cons_n, 'kWh'
    PRINT 300, '  Tarifa     : S/', precio_n
    PRINT 300, '  Costo      : S/', costo_n
    PRINT *, ''
    PRINT *, 'CONSEJO:'
    PRINT *, 'Para ahorrar, programa el uso en horario nocturno'
    PRINT *, 'cuando la tarifa es más económica.'
    
100 FORMAT(1X, A, F12.2, 1X, A)
300 FORMAT(3X, A, F10.2, A)
END SUBROUTINE MOSTRAR_RESULTADOS_TARIFAS

SUBROUTINE MOSTRAR_RESULTADOS_CO2(pot, cons, costo, co2, nombre, &
                                 v, i, h, d, precio)
    REAL, INTENT(IN) :: pot, cons, costo, co2, v, i, h, d, precio
    CHARACTER(LEN=*), INTENT(IN) :: nombre
    
    PRINT *, ''
    PRINT *, '============================================'
    PRINT *, '    RESULTADOS CON HUELLA DE CARBONO'
    PRINT *, '============================================'
    PRINT *, ''
    PRINT *, 'Dispositivo: ', TRIM(nombre)
    PRINT *, '--------------------------------------------'
    PRINT 100, 'Potencia calculada      :', pot, 'W'
    PRINT 100, 'Consumo eléctrico       :', cons, 'kWh/año'
    PRINT 100, 'Costo anual estimado    : S/', costo
    PRINT 100, 'Emisiones de CO2        :', co2, 'kg/año'
    PRINT *, '--------------------------------------------'
    PRINT *, 'EQUIVALENCIAS AMBIENTALES:'
    PRINT *, ''
    PRINT *, 'Arboles necesarios para absorber este CO2:'
    PRINT *, '   Aprox.', NINT(co2 / 21.0), ' árboles'
    PRINT *, '   (1 árbol absorbe ~21 kg CO2/año)'
    PRINT *, ''
    PRINT *, 'Equivalente en kilómetros recorridos:'
    PRINT *, '   Aprox.', NINT(co2 / 0.12), ' km en auto'
    PRINT *, '   (Auto promedio: 0.12 kg CO2/km)'
    PRINT *, ''
    PRINT *, 'RECOMENDACIÓN:'
    IF (co2 > 1000.0) THEN
        PRINT *, 'Consumo ALTO - Considera eficiencia energética'
    ELSE IF (co2 > 500.0) THEN
        PRINT *, 'Consumo MODERADO - Podrías optimizar'
    ELSE
        PRINT *, 'Consumo BAJO - ¡Buen trabajo!'
    END IF
    
100 FORMAT(1X, A, F12.2, 1X, A)
END SUBROUTINE MOSTRAR_RESULTADOS_CO2

SUBROUTINE GUARDAR_HISTORIAL_FILE(nombre, consumo, costo)
    CHARACTER(LEN=*), INTENT(IN) :: nombre
    REAL, INTENT(IN) :: consumo, costo
    CHARACTER(LEN=80) :: fecha_hora
    
    OPEN(UNIT=10, FILE='historial_electrico.txt', &
         STATUS='UNKNOWN', POSITION='APPEND')
    
    ! Obtener fecha/hora simple
    CALL DATE_AND_TIME(fecha_hora)
    
    WRITE(10, 400) TRIM(nombre), consumo, costo, TRIM(fecha_hora)
    CLOSE(10)
    
400 FORMAT(A, ',', F10.2, ',', F10.2, ',', A)
END SUBROUTINE GUARDAR_HISTORIAL_FILE

SUBROUTINE MOSTRAR_HISTORIAL_FILE()
    CHARACTER(LEN=50) :: nombre, fecha
    REAL :: consumo, costo
    INTEGER :: ios, contador
    REAL :: total_consumo = 0.0, total_costo = 0.0
    
    PRINT *, ''
    PRINT *, '============================================'
    PRINT *, '          HISTORIAL DE CÁLCULOS'
    PRINT *, '============================================'
    PRINT *, ''
    
    OPEN(UNIT=10, FILE='historial_electrico.txt', STATUS='OLD', IOSTAT=ios)
    
    IF (ios /= 0) THEN
        PRINT *, 'No hay historial disponible.'
        RETURN
    END IF
    
    PRINT *, 'Dispositivo           Consumo(kWh)   Costo(S/)   Fecha'
    PRINT *, '------------------------------------------------------'
    
    contador = 0
    DO
        READ(10, *, IOSTAT=ios) nombre, consumo, costo, fecha
        IF (ios /= 0) EXIT
        
        PRINT 500, TRIM(nombre), consumo, costo, TRIM(fecha)
        total_consumo = total_consumo + consumo
        total_costo = total_costo + costo
        contador = contador + 1
    END DO
    
    CLOSE(10)
    
    PRINT *, '------------------------------------------------------'
    PRINT 600, 'TOTAL:', total_consumo, total_costo
    PRINT *, ''
    PRINT *, 'Registros encontrados:', contador
    
500 FORMAT(1X, A20, F12.2, F12.2, 4X, A)
600 FORMAT(1X, A20, F12.2, F12.2)
END SUBROUTINE MOSTRAR_HISTORIAL_FILE

SUBROUTINE CONFIGURAR_TARIFAS_PERS()
    PRINT *, ''
    PRINT *, 'Configuración de tarifas personalizadas:'
    PRINT *, '--------------------------------------------'
    
    PRINT *, 'Tarifa horario día (S/kWh):'
    READ *, TARIFA1_DIA
    PRINT *, 'Tarifa horario noche (S/kWh):'
    READ *, TARIFA1_NOCHE
    
    PRINT *, ''
    PRINT *, 'Tarifas actualizadas correctamente'
    PRINT *, 'Día: S/', TARIFA1_DIA, '/kWh'
    PRINT *, 'Noche: S/', TARIFA1_NOCHE, '/kWh'
END SUBROUTINE CONFIGURAR_TARIFAS_PERS

END PROGRAM consumoelectrico_completo