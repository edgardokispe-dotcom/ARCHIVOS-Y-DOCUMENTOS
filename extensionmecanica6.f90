program ergonomia_extension_cabezal
    implicit none

    ! Declaración de variables de entrada (Original)
    real :: altura_individuo          ! Altura de la persona sin calzado (m)
    real :: altura_individuo_calzado  ! Altura total con calzado de trabajo (m)

    ! Declaración de variables de salida (Original)
    real :: altura_cabezal            ! Altura del cabezal ingresada como variable (m)
    real :: altura_calzado            ! Grosor/altura de la suela del calzado (m)
    real :: altura_extension          ! Largo del mango/extensión mecánica (m)
    real :: altura_extension_cabezal  ! Largo total del conjunto (m)

    ! Constantes (Original)
    real, parameter :: DIAMETRO_EXTENSION = 1.0 ! Diámetro fijo del mango en pulgadas

    ! Variables: Sección Extensión Ultra Ergonómica
    real :: seg_a                     ! Longitud del segmento A (m)
    real :: seg_b                     ! Longitud del segmento B (m)
    real :: seg_c                     ! Longitud del segmento C (m)
    real, parameter :: B_CONST = 0.42           ! Sección intermedia constante B = 42 cm (0.42 m)
    real, parameter :: PROPORCION_AC = 2.53     ! Relación de aspecto A/C
    real, parameter :: DIAM_A = 2.5             ! Diámetro segmento A en mm
    real, parameter :: DIAM_B = 3.0             ! Diámetro segmento B en mm
    real, parameter :: DIAM_C = 2.5             ! Diámetro segmento C en mm

    ! Entrada de datos (Original)
    print *, '==========================================================='
    print *, '   SISTEMA DE CALCULO ERGONOMICO: EXTENSION Y CABEZAL'
    print *, '==========================================================='
    print *, 'Ingrese la altura del individuo sin calzado (m):'
    read *, altura_individuo

    print *, 'Ingrese la altura del individuo con calzado de trabajo (m):'
    read *, altura_individuo_calzado

    ! Lectura de la altura del cabezal con validacion de rango (0.10 m a 0.14 m) (Original)
    do
        print *, 'Ingrese la altura del cabezal en metros (rango 0.10 a 0.14):'
        read *, altura_cabezal
        if (altura_cabezal >= 0.10 .and. altura_cabezal <= 0.14) exit
        print *, ' [ERROR] Valor fuera de rango. Debe estar entre 0.10 m y 0.14 m.'
    end do

    ! Cálculos (Original)
    altura_calzado = altura_individuo_calzado - altura_individuo
    altura_extension_cabezal = altura_individuo_calzado
    altura_extension = altura_extension_cabezal - altura_cabezal

    ! Salida de resultados con leyendas ergonómicas (Original)
    print *, '-----------------------------------------------------------'
    print *, 'RESUMEN DE MEDICIONES Y RESULTADOS:'
    print '(A, F5.2, A)', ' Altura del individuo            : ', altura_individuo, ' m'
    print '(A, F5.2, A)', ' Altura del individuo con calzado: ', altura_individuo_calzado, ' m'
    print '(A, F5.2, A)', ' Altura del calzado (resultado)  : ', altura_calzado, ' m'
    print '(A, F5.2, A)', ' Altura del cabezal (resultado)  : ', altura_cabezal, ' m'
    print '(A, F5.2, A)', ' Altura de la extension (result.): ', altura_extension, ' m'
    print '(A, F5.2, A)', ' ALTURA EXTENSION + CABEZAL (RES): ', altura_extension_cabezal, ' m'
    print '(A, F3.1, A)', ' Diametro de la extension (const): ', DIAMETRO_EXTENSION, ' pulgada'
    print *, '-----------------------------------------------------------'
    print *, 'LEYENDAS Y RECOMENDACIONES ERGONOMICAS:'
    print *, ' [!] Ergonomia: La altura combinada de la extension mas el'
    print *, '     cabezal coincide exactamente con la altura del individuo'
    print *, '     calzado para evitar flexiones de columna durante el uso.'
    print *, ' [!] Agarre: El diametro de 1.0 pulgada permite una fuerza'
    print *, '     de prension biomecanica optima para la mano del operario.'
    print *, ' [!] Usos: para uso en limpieza, pintura y otros fines.'

    ! =========================================================================
    ! DIAGRAMA LINEAL BÁSICO (PRIMEROS RESULTADOS)
    ! =========================================================================
    print *, '-----------------------------------------------------------'
    print *, 'DIAGRAMA LINEAL BASICO DEL CONJUNTO (RESULTADOS INICIALES):'
    print *, ''
    print '(A, F4.2, A, F4.2, A)', &
        '|-- Extension (', altura_extension, 'm) --||-- Cabezal (', &
        altura_cabezal, 'm) --|'
    print '(A, F3.1, A)', &
        '|   Diam: ', DIAMETRO_EXTENSION, ' pulgada         ||                     |'
    print *, '+-----------------------+---------------------+ '
    print '(A, F4.2, A, F4.2, A)', &
        '0.00m                  ', altura_extension, &
        'm            ', altura_extension_cabezal, 'm'

    ! =========================================================================
    ! SECCIÓN: EXTENSIÓN ULTRA ERGONÓMICA (SEGMENTOS A, B, C)
    ! =========================================================================
    seg_b = B_CONST
    seg_c = (altura_extension - seg_b) / (PROPORCION_AC + 1.0)
    seg_a = PROPORCION_AC * seg_c

    print *, '==========================================================='
    print *, '      NUEVA SECCION: EXTENSION ULTRA ERGONOMIC'
    print *, '==========================================================='
    print *, ' Desglose colineal de 3 segmentos (A, B, C):'
    print '(A, F5.2, A, F4.1, A)', '  - Segmento A (Base)     : ', seg_a, &
        ' m  (Diametro: ', DIAM_A, ' mm)'
    print '(A, F5.2, A, F4.1, A)', '  - Segmento B (Interm.)  : ', seg_b, &
        ' m  (Diametro: ', DIAM_B, ' mm)'
    print '(A, F5.2, A, F4.1, A)', '  - Segmento C (Superior) : ', seg_c, &
        ' m  (Diametro: ', DIAM_C, ' mm)'
    print '(A, F5.2, A)',          '  - SUMA EXTENSION (A+B+C): ', &
        seg_a + seg_b + seg_c, ' m'
    print *, '-----------------------------------------------------------'
    print *, 'DIAGRAMA LINEAL COLINEAL DEL CONJUNTO:'
    print *, ''
    print '(A, F4.2, A, F4.2, A, F4.2, A, F4.2, A)', &
        '|-- Seg. A (', seg_a, 'm) --||-- Seg. B (', seg_b, &
        'm) --||-- Seg. C (', seg_c, 'm) --||-- Cabezal (', &
        altura_cabezal, 'm) --|'
    print '(A, F4.1, A, F4.1, A, F4.1, A)', &
        '|   Diam: ', DIAM_A, 'mm     ||   Diam: ', DIAM_B, &
        'mm     ||   Diam: ', DIAM_C, 'mm     ||                  |'
    print *, '+--------------------+--------------------+--------------------+-------------------+'
    print '(A, F4.2, A, F4.2, A, F4.2, A, F4.2, A, F4.2, A)', &
        '0.00m               ', seg_a, &
        'm                ', seg_a + seg_b, &
        'm                ', altura_extension, &
        'm             ', altura_extension_cabezal, 'm'
    print *, '-----------------------------------------------------------'
    print *, ' [!] NOTA FINAL:'
    print *, '     SE RECOMIENDA QUE LA EXTENSION MECANICA SEA UN SOLIDO DE INICIO A FIN.'
    print *, '     SEGMENTO B ES CONSTANTE : 0.42 m'

    print *, '==========================================================='

end program ergonomia_extension_cabezal