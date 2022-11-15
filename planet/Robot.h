// Manejo del robot de la sala de control
Process accionRobot;
Process RobotCoger;
Process RobotDejar;
Process RobotMover;
Process RobotDescribir;
Flag posicionRobot; // Localidad donde está el robot actualmente

RESPONSE
{
    * GUSANO|* CRIATURA: PRESENT objRobot WHATO EQ 54 locRobot MESSAGE "Está flotando sobre el robot." DONE;
    * BOTON: PRESENT objPanelControl 
             NOTEQ 44 ARRIBA 
             NOTEQ 44 ABAJO 
             NOTEQ 44 NORTE 
             NOTEQ 44 SUR 
             NOTEQ 44 ESTE 
             NOTEQ 44 OESTE 
             MESSAGE "¿A cuál te refieres?" 
             DONE;
    * BOTON: PRESENT objPanelControl COPYFF 44 34 COPYFF 45 35;
    
    // Aquí las acciones con los botones
    EMPUJA ARRIBA |
    PULSA ARRIBA |
    EMPUJA ABAJO |
    PULSA ABAJO |
    EMPUJA NORTE |
    PULSA NORTE |
    EMPUJA SUR |
    PULSA SUR |
    EMPUJA ESTE |
    PULSA ESTE |
    EMPUJA OESTE |
    PULSA OESTE: PRESENT objPanelControl PROCESS accionRobot DONE;
}

PROCESS accionRobot
{
    _ _: EQ 34 Arriba PROCESS RobotCoger DONE;
    _ _: EQ 34 Abajo PROCESS RobotDejar DONE;
    _ _: COPYFF 34 33 PROCESS RobotMover DONE;
}

Flag origenRobot;

PROCESS RobotMover
{
    _ _ : COPYFF posicionRobot origenRobot 
          MOVE posicionRobot;
    _ _ : EQ posicionRobot TunelSinuoso |      // Localidades "prohibidas"
          EQ posicionRobot PasajeAcolchado
          COPYFF origenRobot posicionRobot;
    _ _ : NOTSAME origenRobot posicionRobot
          COPYFO posicionRobot objRobot
          MESSAGE "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga.";
    _ _ : NOTSAME origenRobot posicionRobot
          PRESENT objRobot
          MESSAGE "El robot aparece en la estancia.";
    _ _ : NOTSAME origenRobot posicionRobot
          SAME origenRobot 38
          MESSAGE "El robot desaparece de la sala deslizándose suavemente.";
    _ _ : SAME origenRobot posicionRobot
          MESSAGE "Una luz {16}{2}roja& parpadea acompañada de un sonido grave. Y luego se apaga.";
          
}

Process RobotCoger // Por este orden, mira si hay: Gusano, Yo, Criatura
{
    _ _ : LET flagPrm1 locRobot Process ContarAtReal NOTZERO flagRet 
          MESSAGE "Una luz {16}{2}roja& parpadea acompañada de un sonido grave. Y luego se apaga." DONE;
    _ _ : PRESENT objRobot PRESENT objGusano MES "El robot emite un sonido agudo, y ves flotar a" MESSAGE "l gusano sobre el robot.";
    //_ _ : PRESENT objRobot PRESENT objCriaturaVerde MES "El robot emite un sonido agudo, y ves flotar a" MESSAGE " la criatura verde sobre el robot.";
    _ _ : LET 34 GUSANO LET 35 255 WHATO SAME posicionRobot 54 PLACE objGusano locRobot MESSAGE "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." DONE;
    _ _ : SAME posicionRobot 38 MESSAGE "¡Eso sería muy peligroso, estás en la misma sala que el robot!" DONE;
    _ _ : LET 34 CRIATURA LET 35 255 WHATO SAME posicionRobot 54 PLACE objCriaturaVerde locRobot MESSAGE "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." SET espejoMovido DONE;
    _ _ : MESSAGE "Una luz {16}{2}roja& parpadea acompañada de un sonido grave. Y luego se apaga.";
}

Process RobotDejar
{
    _ _ : LET flagPrm1 locRobot Process ContarAtReal ZERO flagRet 
          MESSAGE "Una luz {16}{2}roja& parpadea acompañada de un sonido grave. Y luego se apaga." DONE;
    _ _ : PRESENT objRobot ISAT objGusano locRobot MES "El robot emite un sonido agudo, y ves flotar a" MESSAGE "l gusano hasta el suelo.";
    _ _ : PRESENT objRobot ISAT objCriaturaVerde locRobot MES "El robot emite un sonido agudo, y ves flotar a" MESSAGE " la criatura verde hasta el suelo.";
    _ _ : ISAT objGusano locRobot EQ posicionRobot SalaDeBasuras PLACE objGusano EstrechoCubiculo MES "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." MESSAGE " {16}{6}Desde el sur te llega el sonido del succionador funcionando.&" DONE;
    _ _ : ISAT objCriaturaVerde locRobot EQ posicionRobot SalaDeBasuras ISNOTAT objGusano EstrechoCubiculo PLACE objCriaturaVerde EstrechoCubiculo MES "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." MESSAGE " {16}{6}Desde el sur te llega el sonido del succionador funcionando.&" DONE;
    _ _ : ISAT objGusano locRobot COPYFO posicionRobot objGusano MESSAGE "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." DONE;
    _ _ : ISAT objCriaturaVerde locRobot COPYFO posicionRobot objCriaturaVerde MESSAGE "Una luz {16}{4}verde& en el panel parpadea durante unos instantes y luego se apaga." DONE;
    
}

Process 1
{
    _ _: PRESENT objRobot PROCESS RobotDescribir;
}

Process RobotDescribir
{
    _ _ : LET flagPrm1 locRobot PROCESS ContarAtReal ZERO flagRet DONE;
    _ _ : MES "El robot lleva ";
    _ _ : ISAT objGusano locRobot MES "un gusano enorme";
    _ _ : ISAT objCriaturaVerde locRobot MES "una criatura verde";
    _ _ : MES " en suspensión sobre su cabeza. " DONE;
}