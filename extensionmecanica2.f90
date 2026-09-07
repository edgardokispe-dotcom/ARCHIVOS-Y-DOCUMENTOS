program ergonomia_extension_cabezal
    implicit none

    ! Declaración de variables de entrada
    real :: altura_individuo          ! Altura de la persona sin calzado (m)
    real :: altura_individuo_calzado  ! Altura total con calzado de trabajo (m)

    ! Declaración de variables de salida (resultados)
    real :: altura_cabezal            ! Altura del cabezal ingresada como variable (m)
    real :: altura_calzado            ! Grosor/altura de la suela del calzado (m)
    real :: altura_extension          ! Largo del mango/extensión mecánica (m)
    real :: altura_extension_cabezal  ! Largo total del conjunto (m)

    ! Constantes
    real, parameter :: DIAMETRO_EXTENSION = 1.0 ! Diámetro fijo del mango en pulgadas

    ! Entrada de datos
    print *, '==========================================================='
    print *, '   SISTEMA DE CALCULO ERGONOMICO: EXTENSION Y CABEZAL'
    print *, '==========================================================='
    print *, 'Ingrese la altura del individuo sin calzado (m):'
    read *, altura_individuo

    print *, 'Ingrese la altura del individuo con calzado de trabajo (m):'
    read *, altura_individuo_calzado

    ! Lectura de la altura del cabezal con validacion de rango (0.05 m a 0.10 m)
    do
        print *, 'Ingrese la altura del cabezal en metros (rango 0.05 a 0.10):'
        read *, altura_cabezal
        if (altura_cabezal >= 0.05 .and. altura_cabezal <= 0.10) exit
        print *, ' [ERROR] Valor fuera de rango. Debe estar entre 0.05 m y 0.10 m.'
    end do

    ! Cálculos
    altura_calzado = altura_individuo_calzado - altura_individuo
    
    ! El conjunto total debe igualar la altura con calzado
    altura_extension_cabezal = altura_individuo_calzado
    
    ! La extensión mecánica es la altura total menos el cabezal
    altura_extension = altura_extension_cabezal - altura_cabezal

    ! Salida de resultados con leyendas ergonómicas
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
    print *, '==========================================================='

end program ergonomia_extension_cabezal