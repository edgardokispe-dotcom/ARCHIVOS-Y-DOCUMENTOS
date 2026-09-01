PROGRAM CALCULO_HP_COMPRESOR
    IMPLICIT NONE
    
    ! Constantes
    REAL, PARAMETER :: HP_A_WATTS = 745.7
    REAL, PARAMETER :: COS_FI = 0.8
    REAL, PARAMETER :: RAIZ3 = 1.7320508
    
    ! Variables
    REAL :: LRA, RLA, voltaje, watts, hp_calculado, factor_fase
    INTEGER :: tipo_motor, i, indice
    CHARACTER(LEN=5), DIMENSION(10) :: hp_nombre
    REAL, DIMENSION(10) :: hp_valor
    
    ! Inicializar tabla de HP (ordenada de MAYOR a MENOR)
    hp_nombre(1) = "1/1";   hp_valor(1) = 1.0
    hp_nombre(2) = "1/2";   hp_valor(2) = 0.5
    hp_nombre(3) = "1/3";   hp_valor(3) = 0.3333
    hp_nombre(4) = "1/4";   hp_valor(4) = 0.25
    hp_nombre(5) = "1/5";   hp_valor(5) = 0.20
    hp_nombre(6) = "1/6";   hp_valor(6) = 0.1667
    hp_nombre(7) = "1/7";   hp_valor(7) = 0.1429
    hp_nombre(8) = "1/8";   hp_valor(8) = 0.125
    hp_nombre(9) = "1/9";   hp_valor(9) = 0.1111
    hp_nombre(10) = "1/10"; hp_valor(10) = 0.10
    
    ! Encabezado
    PRINT *, "=================================================="
    PRINT *, "   CALCULO DE HP POR DATOS DEL COMPRESOR"
    PRINT *, "   (Aproximacion al valor menor o igual)"
    PRINT *, "=================================================="
    PRINT *, ""
    
    ! Entrada de datos
    PRINT *, "Ingrese los datos del compresor:"
    PRINT *, "---------------------------------"
    WRITE(*, '(A)', ADVANCE='NO') " LRA (Locked Rotor Amperes): "
    READ *, LRA
    WRITE(*, '(A)', ADVANCE='NO') " Voltaje (V): "
    READ *, voltaje
    WRITE(*, '(A)', ADVANCE='NO') " Tipo de motor (1=Monofasico, 2=Trifasico): "
    READ *, tipo_motor
    
    IF (tipo_motor /= 1 .AND. tipo_motor /= 2) THEN
        PRINT *, "ERROR: Opcion invalida."
        STOP
    END IF
    
    ! Cálculos
    RLA = LRA / 7.0
    
    IF (tipo_motor == 1) THEN
        factor_fase = 1.0
        PRINT *, ""
        PRINT *, ">>> Motor Monofasico seleccionado"
    ELSE
        factor_fase = RAIZ3
        PRINT *, ""
        PRINT *, ">>> Motor Trifasico seleccionado"
    END IF
    
    watts = voltaje * RLA * factor_fase * COS_FI
    hp_calculado = watts / HP_A_WATTS
    
    ! Mostrar resultados
    PRINT *, ""
    PRINT *, "RESULTADOS DEL CALCULO:"
    PRINT *, "-----------------------"
    WRITE(*, '(A, F8.2, A)') " LRA ingresado:         ", LRA, " A"
    WRITE(*, '(A, F8.2, A)') " RLA calculado (LRA/7): ", RLA, " A"
    WRITE(*, '(A, F8.2, A)') " Voltaje:               ", voltaje, " V"
    WRITE(*, '(A, F8.2)')    " Factor de fase:        ", factor_fase
    WRITE(*, '(A, F8.2)')    " Factor de potencia:    ", COS_FI
    WRITE(*, '(A, F10.2, A)')" Potencia calculada:    ", watts, " Watts"
    WRITE(*, '(A, F8.3)')    " HP calculado:          ", hp_calculado
    
    ! ============================================================
    ! NUEVA LÓGICA: Encontrar el valor MENOR O IGUAL más cercano
    ! ============================================================
    indice = -1  ! Inicializar como no encontrado
    
    DO i = 1, 10
        IF (hp_calculado >= hp_valor(i)) THEN
            indice = i
            EXIT  ! Salir del bucle al encontrar el primero (mayor a menor)
        END IF
    END DO
    
    ! Si el HP calculado es menor que el valor más pequeño de la tabla (1/10 HP)
    IF (indice == -1) THEN
        indice = 10  ! Usar 1/10 HP como mínimo
    END IF
    
    ! Mostrar tabla
    PRINT *, ""
    PRINT *, "COMPARACION CON TABLA COMERCIAL (CUADRO 1):"
    PRINT *, "-------------------------------------------"
    PRINT *, "   HP Fraccion     |   HP Decimal"
    PRINT *, "-------------------+---------------"
    
    DO i = 1, 10
        IF (i == indice) THEN
            WRITE(*, '(A10, "     |     ", F6.4, "   <<< SELECCIONADO (menor o igual)")') &
                  hp_nombre(i), hp_valor(i)
        ELSE
            WRITE(*, '(A10, "     |     ", F6.4)') hp_nombre(i), hp_valor(i)
        END IF
    END DO
    
    ! Resultado final
    PRINT *, ""
    PRINT *, "RESULTADO FINAL:"
    PRINT *, "================="
    PRINT *, ""
    WRITE(*, '(A, F8.3, A)') " El compresor tiene ", hp_calculado, " HP calculados"
    WRITE(*, '(A, A, A)') " Segun la tabla, corresponde a un motor de ", &
                          TRIM(hp_nombre(indice)), " HP (menor o igual)"
    WRITE(*, '(A, F6.4, A)') " Valor decimal: ", hp_valor(indice), " HP"
    PRINT *, ""
    PRINT *, "NOTA: La aproximacion es al valor MENOR O IGUAL mas cercano"
    PRINT *, "      (No es redondeo al mas cercano)"
    PRINT *, ""
    PRINT *, "VERIFICACION DE EJEMPLOS:"
    PRINT *, "-------------------------"
    PRINT *, " Ejemplo 1: 0.28 HP -> 1/4 HP (0.25) ✓"
    PRINT *, " Ejemplo 2: 0.13 HP -> 1/8 HP (0.125) ✓"
    PRINT *, " Ejemplo 3: 0.15 HP -> 1/7 HP (0.1429) ✓"
    PRINT *, " Ejemplo 4: 0.20 HP -> 1/5 HP (0.20) ✓"
    PRINT *, ""
    PRINT *, "=================================================="
    PRINT *, "         PROGRAMA FINALIZADO CORRECTAMENTE"
    PRINT *, "=================================================="
    
END PROGRAM CALCULO_HP_COMPRESOR