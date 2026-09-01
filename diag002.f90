program diagnosticoneveras

 implicit none 

 real::T1,T2
 character(len=40),parameter::x='CONVERSION A GRADOS CENTIGRADOS',y='CONVERSION A GRADOS FARENHEIT'
 integer :: f,g,h,i,j,l,m,n,p,q,r,s,k,t



!verificar la sentencia read*, y sus variables asociadas,antes de cada condicional
 print*,'!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
 print*,'__________________________________________________________________'
 print*,''
 print*,''
 10 print*,'__________________________________________________________________'
 print*,''
 print*,'MENU PRINCIPAL DE DIAGNOSTICO PARA FALLAS EN EQUIPOS DE FRIO'
 print*,''
 print*,'__________________________________________________________________'
 print*,''
 print*,   'seleccione una opcion y presione INTRO:'
 print*,'___________________________________________'
 print*,''
 print*,'(1)el compresor NO FUNCIONA(no genera ruido)'
 print*,'(2)el compresor NO FUNCIONA(el protector termico actua o funciona)'
 print*,'(3)el compresor FUNCIONA(el protector termico actua o funciona)'
 print*,'(4)el compresor FUNCIONA(trabaja en pequenos ciclos)'
 print*,'(5)la unidad o el compresor FUNCIONA CONTINUAMENTE'
 print*,'(6)temperatura ELEVADA del equipo de refrigeracion'
 print*,'(7)linea de succion CON HIELO o HUMEDAD EXTERNA'
 print*,'(8)ruido'
 print*,'(9)CONVERSION DE TEMPERATURAS'
 print*,'(10)aumento en el CONSUMO DE ENERGIA'
 print*,'(11)fallas MAS COMUNES EN SISTEMAS DE FRIO'
 print*,'(12)fallas MAS COMUNES EN EQUIPOS ' 
 print*,'(13)LISTADO DE COMPRESORES HERMETICOS COMERCIALES'
 print*,'(15)calculo simple para motores hermeticos y refrigeradoras'
 print*,'(14)salir del programa de diagnostico'
 print*,'(16)tecnicas de venta de servicios tecnicos'
 print*,''
 print*,''
 print*,''
 print*,'//reevaluar los diagnosticos por partes : electrica y mecanica//'
 print*,'//y generar un programa paralelo<<incluir conversion presion-temperatura de ref.//'
 print*,'///y diagrama secuencial del circuito de refrigeracion///'
 print*,'///y fugas de corrientes en apartado electrico///'

 12 read(*,*)f


!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 if (f==1) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)falta tension en el enchufe'
  print*,'(2)cable interrumpido'
  print*,'(3)protector termico defectuoso'
  print*,'(4)conexion electrica inadecuada'
  print*,'(5)componentes electricos defectuosos(ej:termostato.)'
  print*,'(6)retornar al menu principal'
    
     
     read(*,*)g    

     if (g==1) then
       print*,'SOLUCION:verifique con un voltimetro'
     end if
     
     if (g==2) then
       print*,'SOLUCION:verifique y/o reemplace el cable' 
     end if
    
     if (g==3) then
       print*,'SOLUCION:verifique y/o remplace el termico'
     end if

     if (g==4) then
       print*,'SOLUCION:verifique el diagrama electrico'
     end if

     if (g==5) then
       print*,'SOLUCION:verifique,regule y/o remmplace el componente'
     end if
     
     if(g==6) then
      goto 10
     end if
     
     

 end if
 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==2) then
   print*,   'seleccione una opcion y presione INTRO'
   print*,'___________________________________________'   
   print*,''
   print*,'(1)conexion inadecuada'
   print*,'(2)baja tension o tension incorrecta'
   print*,'(3)capacitor de arranque defectusoso o incorrecto'
   print*,'(4)rele defectuoso o incorrecto'
   print*,'(5)protector termico no especificado'
   print*,'(6)bobina del motor de compresor sin continuidad'
   print*,'(7)retornar al menu principal'
  
    read*,h
   
     if (h==1) then
       print*,'SOLUCION:verifique la conexion de acuerdo con el diagrama electrico'
     end if
     
     if (h==2) then
       print*,'SOLUCION:aplique o verifique la tension especificada' 
     end if
    
     if (h==3) then
       print*,'SOLUCION:verifique y/o reemplace el capacitor'
     end if

     if (h==4) then
       print*,'SOLUCION:verifique y/o reemplace el rele'
     end if

     if (h==5) then
       print*,'SOLUCION:verifique y/o reemplace el protector termico'
     end if 
     
     if (h==6) then
       print*,'SOLUCION:verifique y/o reemplace el compresor'  
     end if
     
     if(h==7) then
      goto 10
     end if

 end if

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==3) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)bajo voltaje o alto voltaje'
  print*,'(2)protector termico defectuoso'
  print*,'(3)capacitor de marcha defectuoso'
  print*,'(4)corriente electrica excesiva en el termico'
  print*,'(5)carga de gas en exceso'
  print*,'(6)compresor inadecuado al sistema'  
  print*,'(7)retornar al menu principal'
     read(*,*)i   

     if (i==1) then
       print*,'SOLUCION:aplique la tension especificada'
     end if
     
     if (i==2) then
       print*,'SOLUCION:verifique y/o reemplace el protector termico' 
     end if
    
     if (i==3) then
       print*,'SOLUCION:verifiquey/o reemplace el capacitor de marcha'
     end if

     if (i==4) then
       print*,'SOLUCION:verifique el diagrama electrico,el forzador o ventilador si es el caso'
     end if

     if (i==5) then
       print*,'SOLUCION:aplique la carga de gas recomendada por el fabricante'
     end if
     
     if (i==6) then
       print*,'SOLUCION:reemplace por un compresor compatible al equipo'
     end if 
     
     if (i==7) then
      goto 10
     end if

end if

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


 if (f==4) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)protector termico defectuoso'
  print*,'(2)termostato defectuoso'
  print*,'(3)alta presion'
  print*,'(4)carga de gas en exceso'
  print*,'(5)retornar al menu principal'
    
     
     read(*,*)j   

     if (j==1) then
       print*,'SOLUCION:verifique o reemplace el protector termico'
     end if
     
     if (j==2) then
       print*,'SOLUCION:verifique o reemplace el termostato' 
     end if
    
     if (j==3) then
       print*,'SOLUCION:ventilacion insuficiente en el condensador/limpieza del condensador'
     end if

     if (j==4) then
       print*,'SOLUCION:verifique y/o aplique la carga de gas recomendada por el fabricante'
     end if
     
     if (j==5) then
      goto 10
     end if
 end if
 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==5) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)deficiencia de gas'
  print*,'(2)compresor inadecuado al equipo de refrigeracion'
  print*,'(3)aislamiento termico deficiente'
  print*,'(4)evaporador bloqueado(hielo)'
  print*,'(5)condensador sucio'
  print*,'(6)restriccion/falta de limpieza en el equipo de refrigeracion'  
  print*,'(7)termostato defectuoso'   
  print*,'(8)retornar al menu principal'
    read(*,*)l 

     if (l==1) then
       print*,'SOLUCION:verifique y/o corrija la fuga y recargar de acuerdo con recomendacion del fabricante'
     end if
     
     if (l==2) then
       print*,'SOLUCION:verifique y/o reempalce el compresor' 
     end if
    
     if (l==3) then
       print*,'SOLUCION:verifique y/o reemplace el aislamiento termico'
     end if

     if (l==4) then
       print*,'SOLUCION:proceda con el deshielo'
     end if

     if (l==5) then
       print*,'SOLUCION:limpiar el condensador '
     end if
     
     if (l==6) then
       print*,'SOLUCION:limpiar equipo de refrigeracion'
     end if 

     if (l==7) then
       print*,'SOLUCION:verifique y/o reemplace el termostato'
     end if
     
     if(l==8) then
       goto 10
     end if
 end if
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  if (f==6) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)controlador de temepratura desregulado'
  print*,'(2)elemento o controlador de flujo(valvula de expansion o capilar)sub/sobredimensionado'
  print*,'(3)evaporador sub/sobredimensionado'
  print*,'(4)circulacion de aire inadecuada'
  print*,'(5)lampara y/o resistencia de deshielo en funcionamiento continuo'
  print*,'(6)problemas de compresion o del compresor'  
  print*,'(7)retornar al menu principal'
    
    read(*,*)m

     if (m==1) then
       print*,'SOLUCION:regule y/o verifique el controlador de temperatura'
     end if
     
     if (m==2) then
       print*,'SOLUCION:redimensione el elemento' 
     end if
    
     if (m==3) then
       print*,'SOLUCION:redimensione el evaporador'
     end if

     if (m==4) then
       print*,'SOLUCION:verificar la circulacion de aire'
     end if

     if (m==5) then
       print*,'SOLUCION:verifique,repare y/o reemplace el interruptor de lampara y/o el TIMER de deshielo '
     end if
     
     if (m==6) then
       print*,'SOLUCION:redimensione la carga termica y/o remplace el compresor por uno adecuado'
     end if 
     
     if (m==7) then
       goto 10
     end if

 end if
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
 if (f==7) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)exceso de carga de gas'
  print*,'(2)elemento de control de flujo permitiendo el paso excesivo de gas refrigerante'
  print*,'(3)forzador del evaporador defectuoso'
  print*,'(4)EQUIPO FUNCIONA/NO ENFRIA(COMPRESOR conecta y desconecta/fuera tiempo),desconecta por TEMPERATURA' 
  print*,'   ','OBSTRUCCION parcial interna ( aceite/humedad/otras particulas)en el sist.de frio' 
  print*,'(5)retornar al menu principal'

     read(*,*)n  

     if (n==1) then
       print*,'SOLUCION:corrija la carga de gas'
     end if
     
     if (n==2) then
       print*,'SOLUCION:ajuste y/o reempalace el elemento de control de flujo' 
     end if
    
     if (n==3) then
       print*,'SOLUCION:verifique rotacion de forzador/reemplace forzador'
     end if
     
     if(n==4) then       
     print*,'SOLUCION:verificar zona de obstruccion/limpieza del sistema/cambio de componente obstruido'
     end if

     if (n==5) then
      goto 10
     end if
     
    end if
 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==8) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)fijaciones flojas'
  print*,'(2)helice del forzador provocando vibracion'
  print*,'(3)cojinete del forzador desgastado'
  print*,'(4)compresor instalado con gomas amortiguadoras diferentes'
  print*,'(5)tuberia vibrando debido contacto con algun objeto o superficie'
  print*,'(6) compresor con RUIDO interno excesivo'
  print*,'(7)retornar al menu principal'
  
    
    read(*,*)p

     if (p==1) then
       print*,'SOLUCION:localice,asegure y/o cambie las fijaciones'
     end if
     
     if (p==2) then
       print*,'SOLUCION:verificar el balance de las helices o reemplazar' 
     end if
    
     if (p==3) then
       print*,'SOLUCION:reemplace el forzador'
     end if

     if (p==4) then
       print*,'SOLUCION:verificar,ajustar o reemplazar las gomas amortiguadoras por unas adecuadas'
     end if

     if (p==5) then
       print*,'SOLUCION:colocar la conexion o tuberia en forma adecuada'
     end if
     
     if (p==6) then
       print*,'SOLUCION:verificar o remplazar compresor'
     end if 
     
     if(p==7) then
      goto 10
     end if

 end if
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

 if (f==10) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)tension de alimentacion alta'
  print*,'(2)dispositivo de control de temeperatura NO FUNCIONA'
  print*,'(3)calibracion del dispositivo de control de temeperatura'
  print*,'(4)exceso de carga de gas'
  print*,'(5)carga de gas insuficiente'
  print*,'(6)aislamiento termico insuficiente o defectuoso'  
  print*,'(7)compresor inadecuado a equipo de refrigeracion'
  print*,'(8)exceso de infiltracion de calor' 
  print*,'(9)retornar al menu principal'
  
 
    read(*,*)q 

     if (q==1) then
       print*,'SOLUCION:instalacion de estabilizador de voltaje'
     end if
     
     if (q==2) then
       print*,'SOLUCION:verificar dispositivo de control de temperatura y/o reemplazar'
     end if
    
     if (q==3) then
       print*,'SOLUCION:verificar si el dispositivo no esta regulado a nivel maximo'
     end if

     if (q==4) then
       print*,'SOLUCION:verificar si hay formacion de hielo en la linea de succion;verificar'
       print*,'y/o redimensionar carga gas refrigerante'
     end if

     if (q==5) then
       print*,'SOLUCION:verificar/corregir y/o recargar gas refrigerante de acuerdo a especificaciones del fabricante'
     end if
     
     if (q==6) then
       print*,'SOLUCION:reemplazar el aislamiento termico y/o reparar'
     end if 

     if (q==7) then
       print*,'SOLUCION:verificar y/o reemplazar por compresor compatible'
     end if
     
     if(q==8) then
       print*,'SOLUCION:verificar empaquetaduras de puertas y/o aislamiento termico del equipo'
     end if
     
    if(q==9)then
     goto 10
    end if


 end if

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==11) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)dispositivo de control de temeperatura con defecto'
  print*,'(2)carga termica NO DIMENSIONADA correctamente'
  print*,'(3)obstruccion parcial de la conexion o tuberia'
  print*,'(4)obstruccion por HUMEDAD en el tubo capilar'
  print*,'(5)condensador NO DIMENSIONADO correctamente,sin limpieza o sin ventilacion'
  print*,'(6)aislamiento termico defectuoso o insuficiente' 
  print*,'(7)valvulas o filtros SATURADOS'
  print*,'(8)capilar o valvula de expansion NO DIMENSIONADO correctamente/??'
  print*,'(9)compresor NO DIMENSIONADO correctamente al equipo/??' 
  print*,'(10)retornar al menu principal'
    
    read(*,*)r

     if (r==1) then
       print*,'SOLUCION:verificar,reparar y/o regular la fijacion del sensor o bulbo del termostato'
     end if
     
     if (r==2) then
       print*,'SOLUCION:redimensionar la carga'
     end if
    
     if (r==3) then
       print*,'SOLUCION:localizar la obstruccion y reparar'
     end if

     if (r==4) then
       print*,'SOLUCION:verificar si hay hielo en la entrada del evaporador:retirar HUMEDAD del sistema(FILTRO SECADOR)'
     end if

     if (r==5) then
       print*,'SOLUCION:redimensionar,limpiar condensador,verificar sistema de ventilacion'
     end if
     
     if (r==6) then
       print*,'SOLUCION:reparar y/o instalar aislamiento termico'
     end if 

     if (r==7) then
       print*,'SOLUCION:limpiar y/o reemplazar valvula o filtro/??'
     end if
 
     if(r==8) then
       print*,'SOLUCION:redimensionar/reemplazar'
     end if

     if(r==9) then
       print*,'reemplazar por compresor DIMENSIONADO al equipo de refrigeracion'
     end if
     
     if(r==10)then
      goto 10
     end if

 end if
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if (f==12) then 
  print*,   'seleccione una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'(1)FRIO excesivo (enfria mucho)'
  print*,'(2)FRIO deficiente (enfria poco)'
  print*,'(3)equipo de refrigeracion en CORTO CIRCUITO'
  print*,'(4)RUIDOS'
  print*,'(5)HUMEDAD EXTERNA en carcasa(sudor externo en el equipo)'
  print*,'(6)HUMEDAD INTERNA en carcasa(sudor interno en el equipo)'
  print*,'(7)CONSUMO EXCESIVO DE ENERGIA'
  print*,'(8)EQUIPO NO funciona(COMPRESOR no conecta),protector termico no acciona'
  print*,'(9)EQUIPO NO funciona(COMPRESOR no conecta),protector termico acciona'
  print*,'(10)EQUIPO NO funciona(COMPRESOR conecta),protector termico acciona'
  print*,'(11)EQUIPO FUNCIONA/NO ENFRIA(COMPRESOR conecta y desconecta/fuera tiempo),desconecta por protector termico'
  print*,'(12)EQUIPO FUNCIONA/NO ENFRIA(COMPRESOR conecta y desconecta/fuera tiempo),desconecta por TEMPERATURA'
  print*,'(13)retornar al menu principal'

    read(*,*)s

     if (s==1) then
       print*,'1-conexiones electricas erradas/mal estado en el sistema de control electrico'
       print*,'2-termostato no se DESCONECTA'
       print*,'3-termostato regulado en nivel maximo(MAX.FRIO)'
       print*,'4-termostato con SENSOR o bulbo suelto'
       print*,'5-termostato con falla o inadecuado'
       print*,'6-equipo sin parte divisoria interna del congelador'
     end if
     
     if (s==2) then
       print*,'1-la iluminacion interior NO DESCONECTA'
       print*,'2-termostato regulado en nivel MINIMO(MIN.FRIO)'
       print*,'3-termostato con SENSOR o bulbo fuera de POSICION ORIGINAL'
       print*,'4-termostato NO ADECUADO'
       print*,'5-protector termico INCORRECTO'
       print*,'6-protector termico DEFECTUOSO'
       print*,'7-obstruccion parcial de tuberias'
       print*,'8-restriccion por humedad en el tubo capilar'
       print*,'9-condensador saturado de suciedad y/o falto de circulacion de aire'
       print*,'10-aislamiento de la puerta en mal estado'
       print*,'11-posicionamiento NO ADECUADO del equipo'
       print*,'12-equipo con USO y desgaste execivo'
       print*,'13-equipo NO utilizado correctamente'
       print*,'14-aislamiento termico con exceso de humedad'
       print*,'15-aislamiento termico deterirado o defectuso'
       print*,'16-exceso de carga de gas refrigerante en el equipo'
       print*,'17-falta de refrigerante en el sistema'
       print*,'18-fuga de gas refrigerante'
       print*,'19-compresor inadecuado al sistema de frio'
       print*,'20-compresor con BAJA capacidad'



     end if
    
     if (s==3) then
       print*,'1-conexiones electricas erradas/mal estado en el sistema electrico'
       print*,'2-cables o componentes electricos EN CONTACTO con superfices metalicas'
       print*,'3-conexion a TIERRA DESCONECTADO o NO ADECUADO'
       print*,'4-termostato con DEFECTOS O FALLAS'
       print*,'5-compresor en CORTOCIRCIUTO con carcasa y/o otro objeto'
       print*,'6-aislamiento termico con exceso de humedad'

     end if

     if (s==4) then
       print*,'1-termostato generando ruido'
       print*,'2-protector termico INCORRECTO'
       print*,'3-protector termico con DEFECTO O FALLA'
       print*,'4-condensador mal instalado/tubos metalicos en contacto'
       print*,'5-nivelacion incorrecta del equipo o de la base del compresor'
       print*,'6-ruidos provocados por otros componentes'
       print*,'7-compresor apoyado en alguna superficie o estructura del equipo'
       print*,'8-expansion del refrigerante en el evaporador'
       print*,'9-fijacion NO adecuada del compresor'
       print*,'10-compresor con ruido interno'

     end if

     if (s==5) then
       print*,'1-termostato regulado en nivel maximo(MAX.FRIO)'
       print*,'2-aislamiento de la puerta en mal estado'
       print*,'3-posicionamiento NO ADECUADO del equipo'
       print*,'4-humedad relativa del aire MUY ELEVADA (superior al 85%)'
       print*,'5-equipo sin parte divisoria interna del congelador'
       print*,'6-aislamiento termico deterirado o defectuso'
     end if
     
     if (s==6) then
       print*,'1-la iluminacion interior NO DESCONECTA'
       print*,'2-termostato regulado en nivel MINIMO(MIN.FRIO)'
       print*,'3-termostato con DEFECTOS O FALLAS'
       print*,'4-aislamiento de la puerta en mal estado'
       print*,'5-equipo sin parte divisoria interna del congelador'
       print*,'6-equipo con USO y desgaste execivo'
       print*,'7-aislamiento termico deterirado o defectuso'
     end if 

     if (s==7) then
       print*,'1-voltaje MUY ALTO'
       print*,'2-conexiones electricas erradas/mal estado en el sistema electrico'
       print*,'3-la iluminacion interior NO DESCONECTA'
       print*,'4-termostato no se DESCONECTA'
       print*,'5-termostato regulado en nivel maximo(MAX.FRIO)'
       print*,'6-termostato con SENSOR o bulbo suelto'
       print*,'7-termostato NO ADECUADO'
       print*,'8-compresor con ALTO AMPERAJE'
       print*,'9-condensador saturado de suciedad y/o falto de circulacion de aire'
       print*,'10-aislamiento de la puerta en mal estado'
       print*,'11-posicionamiento NO ADECUADO del equipo'
       print*,'12-equipo sin parte divisoria interna del congelador'
       print*,'13-equipo con USO y desgaste execivo'
       print*,'14-aislamiento termico con exceso de humedad'
       print*,'15-exceso de carga de gas refrigerante en el equipo'
       print*,'16-falta de refrigerante en el sistema'
       print*,'17-compresor inadecuado al sistema de frio'
       print*,'18-compresor con BAJA capacidad'
     end if
     
     if(s==8) then
       print*,'1-falta de voltaje (FUENTE DE VOLTAJE)'
       print*,'2-cables o red de cables DESCONECTADOS'
       print*,'3-conexiones electricas erradas/mal estado en el sistema de control electrico'
       print*,'4-componentes electricos sin VOLTAJE al compresor'
       print*,'5-termostato DESCONECTADO'
       print*,'6-termostato sin VOLTAJE entre los contactos'
       print*,'7-protector termico DEFECTUOSOS'
       print*,'8-bobinas de motor compresor con FALLAS'
     end if
 
     if(s==9) then
       print*,'1-voltaje muy BAJO'
       print*,'2-TRANSFORMADOR no ADECUADO'
       print*,'3-termostato con DEFECTOS O FALLAS'
       print*,'4-protector termico NO ADECUADO'
       print*,'5-RELE DE ARRANQUE/PTC no ADECUADO'    
       print*,'6-RELE DE ARRANQUE/PTC con FALLAS'
       print*,'7-CAPACITOR DE ARRANQUE incorrecto'
       print*,'8-CAPACITOR DE ARRANQUE con fallas'
       print*,'9-COMPRESOR con VOLTAJE distinto al especificado'
       print*,'10-bobinas de motor compresor con FALLAS'
       print*,'11-UTILIZACION de una VALVULA DE EXPANSION/*error*'
       print*,'12-compresor TRABADO MECANICAMENTE'
     end if

     if(s==10) then
       print*,'1-voltaje MUY BAJO'
       print*,'2-voltaje MUY ALTO'
       print*,'3-protector termico INCORRECTO o con FALLAS'
       print*,'4-RELE DE ARRANQUE/PTC no ADECUADO'
       print*,'5-RELE DE ARRANQUE/PTC con FALLAS'
       print*,'6-COMPRESOR con VOLTAJE distinto al especificado'
       print*,'7-bobinas de motor compresor con FALLAS' 
       print*,'8-compresor con ALTO AMPERAJE'
       print*,'9-obstruccion parcial de tuberias'
       print*,'10-condensador saturado de suciedad y/o falto de circulacion de aire'
       print*,'11-posicionamiento NO ADECUADO del equipo'
       print*,'12-exceso de carga de gas refrigerante en el equipo'
       print*,'13-UTILIZACION de una VALVULA DE EXPANSION/*error*'
       print*,'14-compresor inadecuado al sistema de frio'
     end if

    if(s==11) then
      print*,'verificar FUGAS DE GAS REFRIGERANTE'
    end if
    
    if(s==12) then
     print*,'OBSTRUCCION parcial interna (aceite/humedad/otras particulas)en el sist.de frio' 
    end if
   
   if(s==13) then
     goto 10
   end if

 end if

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
if (f==13) then 
  print*,'LISTADO DE COMPRESORES HERMETICOS COMERCIALES'
  print*,'220.00 voltios/60 Hz'
  print*,''
  print*,'__________________________________________________'
  print*,''
  print*,'MARCA','      ','CAPACIDAD HP-KW','     ' ,'MODELO','   ','CAPILAR(m)'
  print*,'danfoss','         ','1/8-0.36'       ,'       ','TES4AT','     ',  '4.5 m'
  print*,'danfoss','         ','1/6-0.36'       ,'       ','FR6B'  ,'       ',  '5.0 m'
  print*,'danfoss','         ','1/5-0.36'       ,'       ','FR75B'  ,'      ',  '4.0 m'
  print*,'danfoss','         ','1/4B-0.50'      ,'      ','FR8SB'  ,'      ',  '1.8 m'
  print*,'danfoss','         ','1/4N-0.44'      ,'      ','FR6SB' ,'      ',  '3.5 m'
  print*,'danfoss','         ','1/3N-0.44'      ,'      ','SC10C' ,'      ',  '4.0 m'
  print*,'danfoss','         ','1/3B-0.42'      ,'      ','SC12B' ,'      ',  '5.0 m'
  print*,'danfoss','         ','1/2N-0.50'      ,'      ','SC18C' ,'      ',  '3.5 m'
  print*,'danfoss','         ','1/2B-0.50'      ,'      ','SC18B' ,'      ',  '3.5 m'
  print*,'embraco','         ','1/8-0.31'       ,'       ','K9'    ,'         ',  '3.1 m'
  print*,'enbraco','         ','1/8-0.31'       ,'       ','K11'   ,'        ',  '3.1 m'
  print*,'embraco','         ','1/5-0.36'       ,'       ','K14'   ,'        ',  '4.0 m'   
  print*,'embraco','         ','1/4B-0.50'      ,'      ','FF8.5B','     ',  '1.8 m'
  print*,'embraco','         ','1/4N-0.44'      ,'      ','FF8.5' ,'      ',  '3.5 m'
  print*,'embraco','         ','1/3B-0.42'      ,'      ','FF5.B' ,'      ',  '4.0 m'
  print*,'embraco','         ','1/3N-0.44'      ,'      ','FF1.5' ,'      ',  '5.0 m'
  print*,''
  print*,'___________________________________________________________________________________'
  print*,''
  print*,'tecumseh','        ',      '1/8(BP)'   ,'        ','AE8ZA7','     ','3.7 mx0.031"'
  print*,'tecumseh','        ',      '1/6(BP)'   ,'        ','AE66ZD7','    ','3.7 mx0.031"'
  print*,'tecumseh','        ',      '1/5(BP)'   ,'        ','AE5ZF9' ,'     ','3.1 mx0.031"'
  print*,'tecumseh','        ',      '1/4(BP)'   ,'        ','AE4ZF11','    ','3.7 mx0.036"'
  print*,'tecumseh','        ',      '1/3(BP)'   ,'        ','CAE2412A','   ','3.1 mx0.042"'
  print*,'                               ','VENTILADOR'
  print*,'tecumseh','        ',      '1/3(BP)'   ,'        ','CAE2412A','   ','3.7 mx0.042"'
  print*,'                               ','ENF.ACEITE'
  print*,'tecumseh','        ',      '1/2(BP)'   ,'        ','CAJ2T12' ,'    ','3.4 mx0.055"'
  print*,'tecumseh','        ',      '1/2(BP)'   ,'        ','CAJ2428L','   ','3.8 mx0.064"'
  print*,'                               ','R-502'
  print*,'tecumseh','        ',      '3/4(BP)'   ,'        ','CAJ2446L','   ','3.5 mx0.064"'
  print*,'                               ','R-502' 
  print*,'tecumseh','        ',      '1/5(AP)'   ,'        ','AE59ZF9' ,'    ','2.3 mx0.044"'
  print*,'tecumseh','        ',      '1/4(AP)'   ,'        ','CAE41ZF11','  ','1.6 mx0.044"'
  print*,'tecumseh','        ',      '1/3(AP)'   ,'        ','CAE4440'  ,'    ','2.5 mx0.055"'
  print*,'tecumseh','        ',      '1/2(AP)'   ,'        ','CAJ4461'  ,'    ','3.1 mx0.064"'
  print*,'tecumseh','        ',      '3/4(AP)'   ,'        ','CAJ4492'  ,'    ','2.5 mx0.064"'
  print*,''
  print*,' ___________________________________________________________________________________'
  print*,''
  print*,'sicom'   ,'           ',      '1/20(BP)' ,'       ','AE1320A'  ,'    ','4.3 mx0.025"'
  print*,'sicom'   ,'           ',      '1/12(BP)' ,'       ','AE1332A'  ,'    ','3.7 mx0.025"'
  print*,'sicom'   ,'           ',      '1/8(BP)'  ,'        ','AE1336A'  ,'    ','3.7 mx0.031"'
  print*,'sicom'   ,'           ',      '1/6(BP)'  ,'        ','AE1343A'  ,'    ','3.7 mx0.031"'
  print*,'sicom'   ,'           ',      '1/5(BP)'  ,'        ','AE1360A'  ,'    ','3.1 mx0.031"'
  print*,'sicom'   ,'           ',      '1/4(BP)'  ,'        ','AE1380A'  ,'    ','3.7 mx0.036"'
  print*,'sicom'   ,'           ',      '1/4(BP)'  ,'        ','AE2410A'  ,'    ','3.1 mx0.036"'
  print*,'                               ','ENF.ACEITE'
  print*,'sicom'   ,'           ',      '1/4(BP)'  ,'        ','AE2410A'  ,'    ','2.5 mx0.036"'
  print*,'                               ','VENTILADOR'
  print*,'sicom'   ,'           ',      '1/3(AP)'  ,'        ','AE2413A'  ,'    ','3.7 mx0.042"'
  print*,'                               ','ENF.ACEITE'
  print*,'sicom'   ,'           ',      '1/3(AP)'  ,'        ','AE2413A'  ,'    ','3.1 mx0.042"'
  print*,'                               ','VENTILADOR'
  print*,'sicom'   ,'           ',      '1/3(AP)'  , '        ','AE2415A'  ,'    ','3.1 mx0.042"'
  print*,'sicom'   ,'           ',      '1/2(AP)'  , '        ','AJ2425A'  ,'    ','3.4 mx0.055"'
  print*,'sicom'   ,'           ',      '1/2(AP)'  , '        ','AJ7441A'  ,'    ','3.7 mx0.064"'
  print*,'                                           ','o 1/3 TONEL'
  print*,'sicom'   ,'           ',      '3/4(AP)'  , '        ','AJ7465A'  ,'    ','3.1 mx0.064"'
  print*,'                                           ','o 1/2 TONEL' 
  print*,''
  print*,'_______________________________________________________________________________________'
  print*,''
  print*,'LISTADO DE COMPRESORES HERMETICOS COMERCIALES'
  print*,'110.00 voltios/60 Hz'
  print*,''
  print*,'_______________________________________________________________________________________'
  print*,'MARCA','      ','CAPACIDAD HP-KW','     ' ,'MODELO','   ','CAPILAR(m)'
  print*,''
  print*,'sicom'   ,'           ',       '1/4(AP)','        ','AE3430','     ','1.6 mx0.044"'
  print*,'sicom'   ,'           ',       '1/4(AP)','        ','AE4430','     ','1.6 mx0.044"'
  print*,'sicom'   ,'           ',       '1/3(AP)','        ','AE3440','     ','2.5 mx0.055"'
  print*,'sicom'   ,'           ',       '1/3(AP)','        ','AE4440','     ','2.5 mx0.055"' 
       
  print*,'(1)retornar al menu principal'
  read*,t
  if(t==1)then
   goto 10
  end if
  


end if

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!verificar la sentencia read*, y sus variables asociadas,antes de cada condicional
 if (f==9) then
  print*,'selecciones una opcion y presione INTRO'
  print*,'___________________________________________'
  print*,''
  print*,'1)convertir grados centigrados a farenheit'
  print*,'2)convertir grados farenhiet a centigrados'
     read*,k
       if (k==1) then
         print*,'ingrese valor en grados centigrados:'
           read*,T1
           T2=(1.8*T1)+32
         print*,T2 ,y
       end if
    
      if (k==2) then
        print*,'ingrese valor en grados farenheit:'
         read*,T2  
         T1=(T2-32)/1.8
        print*,T1 ,x
      end if
 
 end if

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  if(f==15) then
   print*,''
   print*,'Calculos para refrigerador neveras congeladoras'
   print*,'==============================================='
   print*,''
   print*,''
   print*,'V= L x A x H = Pies cubicos del refrigerador y se repite la formula para el congelador.'
   print*,''
   print*,'LARGO x ANCHO x ALTO, dividido entre 1728( CONSTANTE)'
   print*,''
   print*,'se suman los dos resultados ( congelador y refrigerador)'
   print*,'De 4 a 6 pies cúbicos = 114 L a 170 L = >1/10 o 1/8'
   print*,'De 7 a 11 pies cúbicos = 198 L a 312 L =>1/6 o 1/5 ligero'
   print*,'De 12 a 13 pies cúbicos = 340 L a 370 L =>1/5 o 1/4 ligero'
   print*,'De 14 a 16 pies cúbicos = 397 L a 453 L =>1/4 pesado'
   print*,'De 17 a 26 pies cúbicos = 481 L a 736 L => 1/3 '
   print*,''
   print*,''
   print*,'1/10 es para una nevera pequeña tipo friobar, frigobar, dispensador de agua.'
   print*,'1/8 corresponderá a una máquina mediana entre 1,10m hasta 1,40m.'
   print*,'1/6  se instalan en máquinas de 1,40m hasta 1,60m; neveras de una puerta y con máquinas que hacen escarcha.'
   print*,'1/5 se instalan en máquinas de 1,60m hasta 1,80m de altura de 2 puertas y en la mayoría de no frost domésticas'
   print*,'de cuerpo angosto'
   print*,'1/4 se instala en máquinas de 1,80m de altura de 2 puertas Side by Side (tipo ropero) anchas.'
   print*,'1/3 vienen en neveras domesticas side by side con ice maker y frigoríficos semi industriales.'
   print*,''
   print*,'LRA = Locked Rotor Amps = amperaje de rotor bloqueado (motor sin movimiento)'
   print*,'RLA = Running Load Amps ó Rated Load Amps = amperaje de carga nominal (motor trabajando)'
  
  end if
  
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 if(f==16) then
     
     print*,'TECNICAS DE VENTAS DE SERVICIOS TECNICOS'
     print*,'' 
     print*,'AIDA'
     print*,''
     print*,'ATENCION DEL CLIENTE-CONQUISTAR'
     print*,'INTERES DEL CLIENTE-ESTIMULAR'
     print*,'DESEO DEL CLIENTE-DESPERTAR'
     print*,'ACCION DE COMPRA DEL SERVICIO-CERRAR VENTA'
     print*,''
     print*,''
     print*,'ACTUALIZACION DE AIDA'
     print*,''
     print*,'SONDEO'
     print*,'ABORDAJE'
     print*,'DEMOSTRACION'
     print*,'INFORMACION'
     print*,'ACCION DE VENTA-CIERRE DE VENTA'
     print*,'SEGUIMIENTO-POST VENTA'
     
  end if
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





  
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print*,'_______________________________________________________________________'
print*,''
print*,''
print*,'_______________________________________________________________________'
print*,'                                            ','the orange jam _tea+milk'
print*,''
print*,''
print*,'_______________________________________________________________________'
 print*,'MENU PRINCIPAL DE DIAGNOSTICO PARA FALLAS EN EQUIPOS DE FRIO'
 print*,''
 print*,'__________________________________________________________________'
 print*,''
 print*,   'seleccione una opcion y presione INTRO:'
 print*,'___________________________________________'
 print*,''
 print*,'(1)el compresor NO FUNCIONA(no genera ruido)'
 print*,'(2)el compresor NO FUNCIONA(el protector termico actua o funciona)'
 print*,'(3)el compresor FUNCIONA(el protector termico actua o funciona)'
 print*,'(4)el compresor FUNCIONA(trabaja en pequenos ciclos)'
 print*,'(5)la unidad o el compresor FUNCIONA CONTINUAMENTE'
 print*,'(6)temperatura ELEVADA del equipo de refrigeracion'
 print*,'(7)linea de succion CON HIELO o HUMEDAD EXTERNA'
 print*,'(8)ruido'
 print*,'(9)CONVERSION DE TEMPERATURAS'
 print*,'(10)aumento en el CONSUMO DE ENERGIA'
 print*,'(11)fallas MAS COMUNES EN SISTEMAS DE FRIO'
 print*,'(12)fallas MAS COMUNES EN EQUIPOS ' 
 print*,'(13)LISTADO DE COMPRESORES HERMETICOS COMERCIALES'
 print*,'(15)calculo simple para motores hermeticos y refrigeradoras'
 print*,'(14)salir del programa de diagnostico'
 print*,'(16)tecnicas de venta de servicios tecnicos'


if(f==14) stop
  
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<








  goto 12
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  

  
  
  
  
  
  
 
!colocar 3 print*,antes y despues de cada CONDICIONAL!!!!

end program diagnosticoneveras


!EN EL PROGRAMA DE DIAGNOSTICO DE EQUIPOS DE REFRIGERACION,AGREGAR OPCIONES DE FALLAS O PROBLEMAS TECNICOS ESPECIFICOS FALTANTES QUE PUEDAN OCURRIR.
!en el programa de diagnostico agregar opciones de fallas o problemas tecnicos especificos faltantes que puedas ocurrir.
!verificar la sentencia read*, y sus variables asociadas,antes de cada condicional
!NOTAS TECNICAS 
 !EL SIMBOLO /?? SIGNIFICA ENCONTRAR LAS CUASAS O EL ORIGEN DEL PROBLEMA.

