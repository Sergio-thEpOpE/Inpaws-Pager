//--------------------------------------------------------------------------------------
// Cosas que se pueden examinar durante el juego
//--------------------------------------------------------------------------------------

Process ExaminaPresent;
RESPONSE
{
    
    #ifndef PAWSPECTRUM
    EX LAGO : AT JuntoLago SYSMESS "La salida del riachuelo forma un peque�o remolino no muy lejos de la orilla.^" DONE;
    EX PINTURA: AT CavidadHumeda SYSMESS "Alguien desciende por una fosa con una cuerda.^" DONE;
    EX HIELO: AT GrutaGlaciar ISNOTAT objBloqueHielo GrutaGlaciar SYSMESS "En algunos sitios tapizan las paredes como planchas de metal, en otros brotan como deformes protuberancias de la fr�a roca.^" DONE;
    EX SIMBOLO: PRESENT objEspejoSalida SYSMESS "Un gran espejo cuadrado tras un duro cristal, orientado formando un �ngulo de 45 grados. Junto a �l, hay un s�mbolo: un cuadrado y una flecha apuntando al interior.^" DONE;
    EX MONITOR: AT SalaComputadora SYSMESS "Un mapa ocupa gran parte de la pantalla. En la parte de arriba ves un complejo de cuevas. La zona inferior representa una zona de hangares o talleres.";
    EX MONITOR: AT SalaComputadora ZERO tanqueHidraulicoLlenado SYSMESS " Un punto rojo parpadea en el lado derecho de los hangares.^" DONE;
    #endif
    
    #ifdef PAWSPECTRUM
    EX LAGO : AT JuntoLago EXTERN 10 DONE;
    EX PINTURA: AT CavidadHumeda EXTERN 15 DONE;
    EX HIELO: AT GrutaGlaciar ISNOTAT objBloqueHielo GrutaGlaciar EXTERN 20 DONE;
    EX SIMBOLO: PRESENT objEspejoSalida EXTERN 25 DONE;
    EX MONITOR: AT SalaComputadora EXTERN 30;
    EX MONITOR: AT SalaComputadora ZERO tanqueHidraulicoLlenado EXTERN 35 DONE;
    EX MONITOR: AT SalaComputadora DONE;
    #endif
    
    EX _ : PROCESS Present NOTZERO flagRet PROCESS ExaminaPresent ZERO flagRet DONE;
}

// Examinar objetos que est�n presentes. Poner a 255 el flagRet para que la respuesta sea un NOTDONE y lo pille la librer�a
// (por ejemplo si queremos que tras la descripci�n se pinte el contenido de un contenedor)

Flag guardaNounExa;
Flag guardaAdjExa;
Process ExaminaPresent
{
	* *: CLEAR flagRet
	      COPYFF 34 guardaNounExa COPYFF 35 guardaAdjExa 
	      PARSE 
	      COPYFF guardaNounExa 34 COPYFF guardaAdjExa 35;
    
    #ifdef PAWSPECTRUM
    _ HIELO: EXTERN 40 DONE;
    _ BOTAS: EXTERN 45 DONE;
    _ FOSA: EXTERN 50 DONE;
    _ ESPEJO: ABSENT objEspejo EXTERN 25 DONE; // Espejo de la salida
    _ ESPEJO: EXTERN 55 DONE;
    _ BABOSA: EXTERN 60 DONE;
    _ CRIATURA: EXTERN 65 DONE;
    _ FUSIBLE: EXTERN 70 DONE; 
    #endif
    
    #ifndef PAWSPECTRUM
    _ BOTAS: SYSMESS "Un poco extra�as, pero est� claro que son para pon�rselas en los pies. Son bastante grandes y resistentes.^" DONE;
    _ HIELO: SYSMESS "Un enorme bloque de hielo, fr�o y deslizante.^" DONE;
    _ BOTAS: SYSMESS "Un poco extra�as, pero est� claro que son para pon�rselas en los pies. Son bastante grandes y resistentes.^" DONE;
    _ FOSA: SYSMESS "La fosa es profunda y oscura, no se ve el fondo.^" DONE;
    _ ESPEJO: ABSENT objEspejo SYSMESS "Un gran espejo cuadrado tras un duro cristal, orientado formando un �ngulo de 45 grados. Junto a �l, hay un extra�o s�mbolo: un cuadrado y una flecha apuntando al interior.^" DONE;
    _ ESPEJO: SYSMESS "Es un espejo corriente de unos 50 cent�metros de longitud.^" DONE;
    _ BABOSA: SYSMESS "Una enorme y repugnante babosa. Al desplazarse deja como un rastro de asquerosa pulpa verde corrosiva.^" DONE;
    _ CRIATURA: SYSMESS "De aspecto antropomorfo, aunque no se podr�a calificar de humana. Aproximadamente 60 cent�metros de longitud, piel verde y abotargada. Piernas cortas, brazos largos con u�as puntiagudas y cabeza achatada.^" DONE;
    _ FUSIBLE: SYSMESS "Bueno, es bastante grande para lo que est�s acostumbrado: unos 50 o 60 cm.^" DONE;
    #endif
    
    
    _ DISPOSITIVO: ZERO campoFuerzaDestruido MESSAGE "En forma de cubo negro, con extra�os s�mbolos y una serie de luces, todas de color rojo." DONE;
    _ DISPOSITIVO: MESSAGE "Un amasijo de hierros retorcidos." DONE;
    _ TECNICO: ZERO estadoTecnico SYSMESS "Lleva un uniforme con m�scara, al igual que el resto de operarios. Est� dormido.^" DONE;
    _ TECNICO: EQ estadoTecnico 1 SYSMESS "Duerme el sue�o de los justos.^" DONE;
    _ PUERTA: PRESENT objPuertaAcero SYSMESS "Junto a ella hay un orificio hexagonal. Est� cerrada.^" DONE;
    _ PISTOLA: SYSMESS "Se utiliza para atontar o dejar sin sentido a criaturas humanoides.^" DONE;
    _ VENTANA: SYSMESS "A trav�s de la ventana ves algo parecido a unos talleres, con multitud de hombrecillos uniformados trabajando.^" DONE;
    _ NAVE: SYSMESS "Peque�a pero potente. En el frontal ves el compartimento del propulsor.^" LET 34 COMPARTIMENTO LET 35 255;
    //_ COMPARTIMENTO: SYSMESS "El compartimento est� " ZERO compartimentoAbierto SYSMESS "cerrado.^" DONE;
    //_ COMPARTIMENTO: SYSMESS "abierto. En su interior hay varios componentes del propulsor, " ISAT objCanalizadorPlasma locCompartimento SYSMESS "incluyendo el canalizador.^" DONE;
    _ CANALIZADOR: SYSMESS "Una pieza fundamental en la propulsi�n de tu nave.^" DONE;
    _ REJILLA: MES "Una especie de respiradero para el basurero de abajo, a bastante altura. " ZERO rejillaSinBarrote MESSAGE "Est� hecha de barrotes muy separados, casi cabes entre ellos. Hay uno un poco suelto." DONE;
    _ REJILLA: SYSMESS "Le falta un barrote, suficiente para caer hacia abajo.^" DONE;
    _ RECIPIENTE: SYSMESS "Est� lleno de un l�quido verde, viscoso y borboteante.^" DONE;
    _ PANEL: SYSMESS "Tiene varios botones, en forma de flecha direccional: norte, sur, este, oeste, arriba y abajo.^" DONE;
    _ ROBOT: SYSMESS "Es b�sicamente un cilindro herm�tico con ruedas.^" PROCESS RobotDescribir CLEAR flagRet DONE;
    _ GUSANO: SYSMESS "Similar a las babosas en forma, que no en tama�o. Es de un color algo m�s tirando al marr�n. No parece muy �gil.^" DONE;
    _ TABLON: SYSMESS "Bastante grande y pesado.^" DONE;
    
	_ _: SET flagRet;
}
