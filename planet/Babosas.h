//-----------------------------------------------------------------------
// CÓDIGO PARA EL MANEJO DE LAS BABOSAS EN LA CAVERNA VERDE
//------------------------------------------------------------------------

Process ExtBabosa;
Process InitBabosas;
Process BabosaDaemon; // Demonio para mover y pintar mensajes de las babosas
Process MueveBabosa; // Mueve el flag que representa la pos de la babosa
Process BabosaVaStr; // La dirección a la que se va o de la que viene la babosa
Process HayBabosa;   // flagRet SET si hay babosa en la localidad destino (flagPrm1)
Process CriaturaVerdeLoc; // Localidad de la criatura Verde
Flag babosaAmenaza;  // la babosa me amenaza en la localidad babosaAmenaza, o cero si no hay amenaza


Flag PROP_BABOSA;
Flag LOCA_BABOSA;
Process ExtBabosa
{
    * * : CLEAR PROP_BABOSA CLEAR LOCA_BABOSA;
}

VOCABULARY { Noun: "BABOSA"; }

Flag LocBabosa1;
Object objBabosa1
{
    "babosa verde";
    WEIGHT 2;
    WORDS BABOSA _ ;
    INITIALLYAT CavernaVerde4;
} 

Flag LocBabosa2;
Object objBabosa2
{
    "babosa verde";
    WEIGHT 2;
    WORDS BABOSA _ ;
    INITIALLYAT CavernaVerde4;
}

//~ Flag LocBabosa3;
//~ Object objBabosa3
//~ {
    //~ "babosa verde";
    //~ WEIGHT 2;
    //~ WORDS BABOSA _ ;
    //~ INITIALLYAT CavernaVerde5;
//~ }

Process ExtBabosa
{
    _ _ : EQ 51 objBabosa1 SET PROP_BABOSA COPYFF LocBabosa1 LOCA_BABOSA;
    _ _ : EQ 51 objBabosa2 SET PROP_BABOSA COPYFF LocBabosa2 LOCA_BABOSA;
//    _ _ : EQ 51 objBabosa3 SET PROP_BABOSA COPYFF LocBabosa3 LOCA_BABOSA;
}

Process InitBabosas
{
    _ _ : LET LocBabosa1 CavernaVerde4 PLACE objBabosa1 CavernaVerde4
          LET LocBabosa2 CavernaVerde3 PLACE objBabosa2 CavernaVerde3;
          //LET LocBabosa3 CavernaVerde5 PLACE objBabosa3 CavernaVerde5;
}

// En flagPrm1 el número de babosa: 1, 2, ...
Flag numBabosa;
Flag origen;
Flag destino;
Flag completarTextoBabosa;
Process BabosaACriatura;    // La babosa va para la criatura

Process BabosaDaemon
{
    _ _ : CLEAR completarTextoBabosa COPYFF flagPrm1 numBabosa;
    _ _ : EQ numBabosa 1 LET 51 objBabosa1; 
    _ _ : EQ numBabosa 2 LET 51 objBabosa2;
 //   _ _ : EQ numBabosa 3 LET 51 objBabosa3;
    
    _ _ : PROCESS ExtBabosa COPYFF LOCA_BABOSA origen COPYFF origen flagPrm1 PROCESS mueveBabosa;
    _ _ : LT flagRet 3 COPYFF origen destino;
    _ _ : GT flagRet 2 COPYFF flagRet destino;
    _ _ : ZERO flagRet SAME 38 origen MESSAGE "La babosa intenta reptar hasta la salida, pero al llegar al espejo cambia repentinamente de dirección.";
    _ _ : EQ flagRet 1 PROCESS BabosaACriatura; 
    _ _ : EQ flagRet 2 SAME 38 origen COPYFF 38 babosaAmenaza MESSAGE "La babosa se dirige hacia tí.";
    _ _ : SAME 38 origen NOTSAME origen destino MES "La babosa repta hacia la salida del " COPYFF destino flagPrm1 PROCESS BabosaVaStr SET completarTextoBabosa; 
    _ _ : SAME 38 destino NOTSAME origen destino MES "Una babosa llega reptando desde el " COPYFF origen flagPrm1 PROCESS BabosaVaStr SET completarTextoBabosa;
    
    _ _ : NOTSAME origen destino COPYFF destino flagPrm1 PROCESS HayBabosa 
          NOTZERO flagRet COPYFF origen destino NOTZERO completarTextoBabosa 
          MESSAGE ", pero antes de llegar, algo la hace darse la vuelta.";
    _ _ : NOTSAME origen destino ZERO flagRet NOTZERO completarTextoBabosa SYSMESS ".^";
    
    _ _ : EQ numBabosa 1 COPYFF destino LocBabosa1 COPYFO destino objBabosa1;
    _ _ : EQ numBabosa 2 COPYFF destino LocBabosa2 COPYFO destino objBabosa2;
//    _ _ : EQ numBabosa 3 COPYFF destino LocBabosa3 COPYFO destino objBabosa3;
}

PROCESS 1
{
    _ _ : NOTSAME babosaAmenaza 38 CLEAR babosaAmenaza;
}
PROCESS 2
{
    _ _ : NOTZERO babosaAmenaza NOTCARR objEspejo MESSAGE "La babosa se te sube y te rocía con todos sus fluidos putrefactos y corruptores." SCORE TURNS END;
    _ _ : NOTZERO babosaAmenaza CLEAR babosaAmenaza MESSAGE "La babosa llega hasta casi tocarte, pero de repente se da la vuelta y se aleja.";
}

// MUEVEBABOSA: Proceso para mover una babosa
// Entrada: flagPrm1, la localidad actual
// Salida: flagRet, la localidad destino, o
//      0: Intenta salir del recinto pero al ver el espejo no se mueve
//      1: Se dirige hacia la criatura
//      2: Se dirige hacia mi
Flag randomBabosa;
Process MueveBabosa
{
    //_ _ : MES "Babosa mueve desde " PRINT flagPrm1; // DEBUG
    _ _ : COPYFF flagPrm1 flagRet; // Por defecto no se mueve
    _ _ : CHANCE 30 DONE; // 30% de los turnos no hace nada
    _ _ : PROCESS CriaturaVerdeLoc
            SAME flagPrm1 locCriaturaVerde // ¿Está aquí la criatura?
            CHANCE 50 
            LET flagRet 1         // Se dirige hacia la criatura
            DONE;
    
    _ _ : SAME flagPrm1 38         // Si está aquí
            CHANCE 50           
            LET flagRet 2         // Se dirige hacia mí
            DONE;
            
    _ _ : RANDOM randomBabosa;
    
    // Salidas desde CavernaVerde (ppal)
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde LT randomBabosa 26 CLEAR randomBabosa LET flagRet CavernaVerde5;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde GT randomBabosa 25 LT randomBabosa 51 CLEAR randomBabosa LET flagRet CavernaVerde4;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde GT randomBabosa 50 LT randomBabosa 76 CLEAR randomBabosa LET flagRet CavernaVerde2;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde CLEAR randomBabosa CLEAR flagRet;
    
    // Salidas desde CavernaVerde5
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde5 LT randomBabosa 51 CLEAR randomBabosa LET flagRet CavernaVerde;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde5 CLEAR randomBabosa LET flagRet CavernaVerde4;
    
    // Salidas desde CavernaVerde4
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde4 LT randomBabosa 34 CLEAR randomBabosa LET flagRet CavernaVerde5;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde4 GT randomBabosa 66 CLEAR randomBabosa LET flagRet CavernaVerde;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde4 CLEAR randomBabosa CLEAR flagRet;
    
    // Salidas desde CavernaVerde2
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde2 LT randomBabosa 34 CLEAR randomBabosa LET flagRet CavernaVerde;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde2 GT randomBabosa 66 CLEAR randomBabosa LET flagRet CavernaVerde3;
    _ _ : NOTZERO randomBabosa EQ flagPrm1 CavernaVerde2 CLEAR randomBabosa CLEAR flagRet;
    
    // Salidas desde CavernaVerde3
    _ _ : NOTZERO randomBabosa CLEAR randomBabosa LET flagRet CavernaVerde2;
    
}

PROCESS BabosaACriatura
{
    _ _ : SAME 38 origen MES "La babosa se dirige hacia la criatura, ";
    _ _ : ZERO espejoMovido SAME 38 origen MESSAGE "pero al acercarse al espejo se da la vuelta y continua reptando por la estancia." DONE;
    _ _ : NOTZERO espejoMovido SWAP objCriaturaVerde objEsqueletoPulposo SAME 38 origen MESSAGE " se sube a ella y en cuestión de segundos la desintegra con sus ácidos fluidos. Ahora sólo queda un esqueleto putrefacto y viscoso." DONE;
}

// En flagPrm1 la dirección origen/destino
Process BabosaVaStr
{
    _ _ : AT CavernaVerde4 EQ flagPrm1 CavernaVerde MES "este";
    _ _ : AT CavernaVerde4 EQ flagPrm1 CavernaVerde5 MES "norte";
    
    _ _ : AT CavernaVerde5 EQ flagPrm1 CavernaVerde4 MES "sur";
    _ _ : AT CavernaVerde5 EQ flagPrm1 CavernaVerde MES "sureste";
    
    _ _ : AT CavernaVerde EQ flagPrm1 CavernaVerde5 MES "noroeste";
    _ _ : AT CavernaVerde EQ flagPrm1 CavernaVerde4 MES "oeste";
    _ _ : AT CavernaVerde EQ flagPrm1 CavernaVerde2 MES "sur";
    
    _ _ : AT CavernaVerde2 EQ flagPrm1 CavernaVerde MES "norte";
    _ _ : AT CavernaVerde2 EQ flagPrm1 CavernaVerde3 MES "este";
    
    _ _ : AT CavernaVerde3 EQ flagPrm1 CavernaVerde2 MES "oeste";
}

Process HayBabosaLoop;
Flag guardaLocActual;
Process HayBabosa   // flagRet SET si hay babosa en la localidad destino (flagPrm1)
{
    _ _ : PROCESS guardaFirst CLEAR flagRet COPYFF 38 guardaLocActual COPYFF flagPrm1 38;
    _ _ : PROCESS HayBabosaLoop;
    _ _ : COPYFF guardaLocActual 38 PROCESS RestauraFirst;
}

Process HayBabosaLoop
{
    _ _ : DOALL 255;
    _ _ : WHATO Process ExtBabosa NOTZERO PROP_BABOSA SET flagRet;
}

Flag locCriaturaVerde;
Process CriaturaVerdeLoc
{
    _ _: PROCESS guardaFirst LET 34 CRIATURA LET 35 VERDE WHATO COPYFF 54 locCriaturaVerde 
         PROCESS RestauraFirst;
}
