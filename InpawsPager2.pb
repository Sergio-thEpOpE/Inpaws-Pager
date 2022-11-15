;Inpaws pagger
;
;A Prepocessed Inpaws compiler For Zx Spectrum 128kb
;


Enumeration
  #C_ALL
  #C_CHARACTERS
  #C_GRAPHICS
  #C_GFX
EndEnumeration

Structure STexto
  tamano.l
  *mensaje
EndStructure

Structure SEnlace
  verbo.a
  destino.a
EndStructure

Structure SConexion
  List enlace.SEnlace()
EndStructure

Structure SVocablo
  numero.l
  tipo.l
  palabra.s
  keytosort.s
EndStructure

Structure SObjeto
  nombre.l
  adjetivo.l
  inicialmente.l
  peso.l
EndStructure

Structure SGrafico
  atributo.l
  tamano.l
  List bytecoded.a()
EndStructure

Structure SEntrada
  key_para_ordenar.s
  verbo.l
  nombre.l
  List condactos.a()
  hash.s
EndStructure

Structure SEntradaOffset
  verbo.l
  nombre.l
  offset.l
EndStructure

Structure SProceso
  numero.l
  List entrada.SEntrada()
EndStructure

Structure SDefaults
  charset.a
	ink.a
	paper.a
	flash.a
	bright.a
	inverse.a
	over.a
	border.a
EndStructure

Structure SCondacto
  numero.l
  num_argumentos.l
EndStructure

Structure SComunesToDump
  List procesos.SProceso()
  Map objetos.STexto()
  Map localidades.STexto()
  Map mensajes.STexto()
  Map sysmensajes.STexto()
  Map conexiones.SConexion()
  List vocabulario.SVocablo()
  Map objetos_inicial.SObjeto()
  List graficos.SGrafico()
EndStructure

Global Dim memoria.a(7,65535)  ;creo páginas 0 a 7; y uso por defecto todo el espacio direccionable
Global Dim paginas_localidades(7)  ;aquí llevo la localidad inicial de cada página
Global Dim paginas_mensajes(7)     ;aqui llevo el mensaje inicial de cada página

NewMap localidades.STexto()
NewMap objetos.STexto()
NewMap mensajes.STexto()
NewMap sysmensajes.STexto()
NewMap objetos_inicial.SObjeto()
NewMap conexiones.SConexion()
NewList vocabulario.SVocablo()
NewList graficos.SGrafico()
NewList udg()
NewList fonts()
NewList procesos.SProceso()
NewMap abreviaturas.STexto()

NewMap condactos_defs.SCondacto()
defaults.SDefaults

Procedure poke_w(pagina,direccion,valor)
  memoria(pagina,direccion) = valor % 256
  memoria(pagina,direccion + 1) = Int(valor/256)
EndProcedure

Procedure poke(pagina,direccion,valor)
  memoria(pagina,direccion) = valor
EndProcedure

Procedure peek_w(pagina, direccion)
  ProcedureReturn memoria(pagina, direccion) + 256*memoria(pagina,direccion + 1)
EndProcedure

Procedure peek(pagina, direccion)
  ProcedureReturn memoria(pagina,direccion)
EndProcedure


Procedure low_byte(word)
  ProcedureReturn word % 256
EndProcedure

Procedure hight_byte(word)
  ProcedureReturn Int(word / 256)
EndProcedure

Procedure nextpagina(pagina)
  pagina + 1
  Select pagina
    Case 2  ;la pagina 2 se salta porque esta siempre paginada en $8000
      pagina = 3
    Case 5  ;la pagina 5 se salta porque está siempre paginada en $4000
      pagina = 6
  EndSelect
  ProcedureReturn pagina
EndProcedure

Procedure isnumber(cadena.s)
  For g=1 To Len(cadena.s)
    If Asc(Mid(cadena.s,g,1)) < 48 Or Asc(Mid(cadena.s,g,1))>57
      ProcedureReturn #False
    EndIf
  Next g
  ProcedureReturn #True
EndProcedure

;avanza el puntero saltando los espacios, tabuladores y saltos de linea
Procedure trimspace(*puntero)
  caracter = PeekA(*puntero)
  While caracter = 32 Or caracter = 9 Or caracter = 10 Or caracter = 13
    *puntero + 1
    caracter = PeekA(*puntero)
  Wend
  ProcedureReturn *puntero
EndProcedure

;devuelve el puntero avanzado, y una cadena con la palabra hasta que encuentra el separador
Procedure getpalabra(*puntero, *salida.String, separador.s)
  *salida\s = ""
  caracter = PeekA(*puntero)
  While caracter <> Asc(separador.s) And caracter <> 9 And caracter <> 10 And caracter <> 13 And caracter <> 32
    *salida\s = *salida\s + Chr(caracter)
    *puntero + 1
    caracter = PeekA (*puntero)
  Wend
  ProcedureReturn *puntero
EndProcedure

Procedure getliteral(*puntero, *mensaje.STexto)
  *temporal = AllocateMemory(5000) ;5k de memorias es una barbaridad, pero más que suficiente para crear la cadena
  *puntero_escribiendo = *temporal
  Repeat
    caracter.s = Chr(PeekA(*puntero))
    *puntero + 1
    Select caracter.s
      Case "{"  ;inicio de un marcador numérico, lo leo evaluo hasta el siguiente "}"
        numero.s = ""
        Repeat
          caracter.s = Chr(PeekA(*puntero))
          numero.s = numero.s + caracter.s
          *puntero + 1
        Until caracter.s = "}"
        PokeA(*puntero_escribiendo, Val(numero.s))
        *puntero_escribiendo + 1
      Case "^"  ;este caracter se sustituye por un chr(7) que el paws lo identifica como salto de linea
        PokeA(*puntero_escribiendo, 7)
        *puntero_escribiendo + 1
      Case Chr(34)
        PokeA(*puntero_escribiendo,31)  ;meto el marcador de final de cadena que usa Paws
        tamano = *puntero_escribiendo - *temporal + 1 ;sumo uno para contar el marcador final de cadena
        ;ahora que sabemos el tamano, reservamos solo la memoria necesaria
        *mensaje\mensaje = AllocateMemory(tamano)
        CopyMemory(*temporal,*mensaje\mensaje,tamano) ;copiamos de la temporal a esta
        FreeMemory(*temporal) ;y liberamos la memoria temporal
        *mensaje\tamano = tamano
        Break ;salimos del bucle repeat
      Case "\"
        especialc = PeekA(*puntero)
        PokeA(*puntero_escribiendo,especialc)
        *puntero + 1
        *puntero_escribiendo + 1
      Default
        PokeA(*puntero_escribiendo,Asc(caracter.s))
        *puntero_escribiendo + 1
    EndSelect
  ForEver
  ProcedureReturn *puntero
EndProcedure

Procedure FinError (mensaje.s)
  Shared control
  Shared in_archivo$
;   If control
;     DeleteFile(in_archivo$)
;   EndIf
  PrintN("[!] "+ mensaje.s)
  End 1
EndProcedure

Procedure IdentifySna(fichero)
  ;
  ;Se revisa que el tamano sea el de un sna valido
  ;
  ;Devuelve 48, si es modo 48
  ;De otro modo, devuelve el valor de la ram que esta paginada en el momento del snapshot
  ;
  tamano=Lof(fichero)
  If tamano<>49179 And tamano<>131103 And tamano<>147487
    ProcedureReturn -1
  ElseIf tamano=49179
    ProcedureReturn 48
  Else
    ;leo el valor de ram paginada
    FileSeek(fichero,49181)
    ram=ReadAsciiCharacter(fichero)&$07
    ProcedureReturn ram
  EndIf
EndProcedure

Procedure IdentifyTap(fichero)
  tamano = Lof(fichero)
  puntero = 0
  FileSeek(fichero, 0)
  buffer = AllocateMemory(tamano)
  ReadData(fichero, buffer, tamano)
  ;ahora repasamos la coherencia del archivo
  While puntero<tamano
    bloque_size = PeekU(buffer + puntero)
    puntero = puntero + 2 + bloque_size
  Wend
  FreeMemory(buffer)
  If puntero = tamano
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure

Procedure LoadMemoriaFromSna(tipo,archivo)
  Shared memoria()
  Dim bancos_leidos(7)
  ;Esta rutina carga los bancos de memoria del spectrum
  ;genera todo un bloque de memoria
  ;
  
  FileSeek(archivo,27)
  ;paginas 5 y 2 [fijas] y las situamos como si fueran pagina 0
  For g=16384 To 49151
    memoria(0,g) = ReadAsciiCharacter(archivo)
  Next g
  bancos_leidos(5) = 1
  bancos_leidos(2) = 1
  ;ahora segun el tipo de snap cargamos la memoria paginada en #c000
  If tipo=48
    For g = 49142 To 65535
      memoria(0,g)=ReadAsciiCharacter(archivo)
    Next g
    bancos_leidos(0) = 1
  Else
    For g= 49152 To 65535
      memoria(tipo,g) = ReadAsciiCharacter(archivo)
    Next g
    bancos_leidos(tipo) = 1
    FileSeek(archivo,49183)
    For z=0 To 7
      If bancos_leidos(z) <> 1
        For g=49152 To 65535
          memoria(z,g) = ReadAsciiCharacter(archivo)
        Next g
        bancos_leidos(z) = 1
      EndIf
    Next z
  EndIf
EndProcedure

Procedure LoadMemoriaFromTap(archivo)
  ;carga los bloques de un TAP que sean consecutivos y que terminen con A,B,C,D, etc....
  Shared memoria()
  FileSeek(archivo, 0)
  caracter = 65 ;el ascii code de la letra A
  pagina = 0
  While Not Eof(archivo)
    bloque_size = ReadUnicodeCharacter(archivo)
    If bloque_size = 19
      flag = ReadAsciiCharacter(archivo)
      typo = ReadAsciiCharacter(archivo)
      nombre$ = ""
      For g = 1 To 10: nombre$ = nombre$ + Chr(ReadAsciiCharacter(archivo)): Next g
      datalong = ReadUnicodeCharacter(archivo)
      database = ReadUnicodeCharacter(archivo)
      dummy = ReadUnicodeCharacter(archivo)
      crc = ReadAsciiCharacter(archivo)
      If flag = 0 And typo = 3 ;si es una cabecera de un bloque de datos avanzamos al siguiente bloque para ver si es el suyo
        If Mid(nombre$,10,1) = Chr(caracter) ;si el último caracter es "A","B" ... etc
          bloque_size_data = ReadUnicodeCharacter(archivo)
          If bloque_size_data = datalong + 2  And ReadAsciiCharacter(archivo) = 255 ;+2 para incluir el flag, y revisamos que el flag sea 255
                                                                                    ;leemos el bloque de data en la memoria que le corresponde
            For g=0 To datalong - 1
              memoria(pagina, database + g) = ReadAsciiCharacter(archivo)
            Next g
            ReadAsciiCharacter(archivo) ;avanzamos el seek para saltar el CRC (ojo, no verifico que sea correcto)
            caracter + 1
            ;si caracter es impar, hay que cambiar de página
            If caracter & 1
              pagina = nextpagina(pagina)
            EndIf
          EndIf
        EndIf
      EndIf
    Else
      FileSeek(archivo, Loc(archivo) + bloque_size)
    EndIf
  Wend
EndProcedure

Procedure CargaDefaults(*puntero,*defaults.SDefaults)
  *puntero = trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero,@leida,"")
  If leida\s <> "{"
    FinError("Missing { on Defaults section")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")  ;leemos el codigo de la palabra
      *puntero + 1                                ;avanzamos los dos puntos
      *puntero = trimspace(*puntero)
      Select leida\s
        Case "CHARSET"
          *puntero = getpalabra(*puntero,@leida,";")  ;leemos el numero hasta el ;
          *defaults\charset = Val(leida\s)            ;lo almacenamos
          *puntero + 1  ;saltamos el ;
        Case "INK"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\ink = Val(leida\s)
          *puntero + 1
        Case "PAPER"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\paper = Val(leida\s)
          *puntero + 1
        Case "FLASH"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\flash = Val(leida\s)
          *puntero + 1
        Case "BRIGHT"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\bright = Val(leida\s)
          *puntero + 1
        Case "INVERSE"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\inverse = Val(leida\s)
          *puntero + 1
        Case "OVER"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\over = Val(leida\s)
          *puntero + 1
        Case "BORDER"
          *puntero = getpalabra(*puntero,@leida,";")
          *defaults\border = Val(leida\s)
          *puntero + 1
        Case "}"
          Break
        Default
          FinError("Wrong sintaxis on Defaults section")
      EndSelect
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaText(*puntero,Map texto.STexto())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Text")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")  ;leemos el numero del mensaje
      If leida\s <> "}"
        *puntero + 3                                ;avanzamos los dos puntos el espacio y las primeras comillas
        contenido.STexto
        *puntero = getliteral(*puntero, @contenido)
        texto(leida\s)\mensaje = contenido\mensaje
        texto()\tamano = contenido\tamano
        *puntero + 1  ;tras el literal y el cierre de comiññas podemos avanzar el ;
      Else
        Break ;salimos si se alcanzó el } (el puntero lector estará justo a continuación)
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaConexiones(*puntero, Map conexiones.SConexion())
  *puntero = trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida, "")
  If leida\s <> "{"
    FinError("Missing { on Conections")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")
      If leida\s <> "}"
        If Not FindMapElement(conexiones(),leida\s)
          AddMapElement(conexiones(),leida\s)
        EndIf
        *puntero + 2  ;saltamos los dos puntos y el espacio
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,"")
        AddElement(conexiones()\enlace())
        conexiones()\enlace()\verbo = Val(leida\s)
        *puntero + 3 ;avazamod " TO "
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,";")
        conexiones()\enlace()\destino = Val(leida\s)
        *puntero + 1 ;avanzamos el ;
      Else
        Break
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaVocabulario(*puntero, List vocabulario.SVocablo())
  *puntero = trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError ("Missing { on Vocabulary")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero, @leida,",") ;leemos el numero hasta la coma
      If leida\s <> "}"
        AddElement(vocabulario())
        vocabulario()\numero = Val(leida\s)
        *puntero + 1  ;avanzamos la ,
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero,@leida,":")
        Select leida\s
          Case "RESERVED"
            vocabulario()\tipo = 255
          Case "VERB"
            vocabulario()\tipo = 0
          Case "ADVERB"
            vocabulario()\tipo = 1
          Case "NOUN"
            vocabulario()\tipo = 2
          Case "ADJECTIVE"
            vocabulario()\tipo = 3
          Case "PREPOSITION"
            vocabulario()\tipo = 4
          Case "CONJUNCTION"
            vocabulario()\tipo = 5
          Case "PRONOUN"
            vocabulario()\tipo = 6
          Default
            FinError("Wrong type of word in vocabulary: " + leida\s)
        EndSelect
        *puntero + 1  ;avanzamos los :
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida, ";") ;leemos la palabra entera (debe ir entre comillas)
        If Left(leida\s, 1) <> Chr(34) Or Right(leida\s, 1) <> Chr(34)
          FinError("Wrong format on word: " + leida\s)
        Else
          vocabulario()\palabra = LSet(Mid(leida\s, 2, Len(leida\s) - 2), 5, " ")
        EndIf
        *puntero + 1 ;avanzamos el ; final
      Else
        Break
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaObjetosInicial (*puntero, Map objetos.SObjeto())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Initiallyat")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")  ;leemos el numero del mensaje
      If leida\s <> "}"
        *puntero + 2                                ;avanzamos los dos puntos el espacio
        objetoreferencia.s = leida\s
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,";")
        objetos(objetoreferencia.s)\inicialmente = Val(leida\s)
        *puntero + 1  ;avanza el ;
      Else
        Break ;salimos si se alcanzó el } (el puntero lector estará justo a continuación)
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaObjetosPalabras (*puntero, Map objetos.SObjeto())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Objects words")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")  ;leemos el numero del mensaje
      If leida\s <> "}"
        *puntero + 2                                ;avanzamos los dos puntos el espacio
        objetoreferencia.s = leida\s
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,",")
        *puntero + 1  ;avanza la ,
        nombre.s = leida\s
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,";")
        FindMapElement(objetos(),objetoreferencia.s)
        objetos()\nombre = Val(nombre.s)
        objetos()\adjetivo = Val(leida\s)
        *puntero + 1  ;avanza el ;
      Else
        Break ;salimos si se alcanzó el } (el puntero lector estará justo a continuación)
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaObjetosPeso (*puntero, Map objetos.SObjeto())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Objects weight")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")  ;leemos el numero del mensaje
      If leida\s <> "}"
        *puntero + 2                                ;avanzamos los dos puntos el espacio
        objetoreferencia.s = leida\s
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,";")
        FindMapElement(objetos(),objetoreferencia.s)
        objetos()\peso = Val(leida\s)
        *puntero + 1  ;avanza el ;
      Else
        Break ;salimos si se alcanzó el } (el puntero lector estará justo a continuación)
      EndIf
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaGraficos (*puntero, List graficos.SGrafico())
  ;se entiend que se cargan los N primeros, que corresponden a las localidades 0..N-1
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Graphics")
  Else
    *puntero = trimspace(*puntero)
    *puntero = getpalabra(*puntero,@leida,",")  ;leemos el numero del localidades presentes
    If leida\s <> "}"
      *puntero + 1  ;avanzamos la coma
      numlocalidades = Val(leida\s)
      ;una vez leida el numero de localidades, leemos el tamano de cada grafico
      For g = 0 To numlocalidades - 1
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,",")
        *puntero + 1 ;avanzamos la coma
        If isnumber(leida\s)
          AddElement(graficos())
          graficos()\tamano = Val(leida\s)
        Else
          FinError("Missing number")
        EndIf
      Next
      ;ahora leemos los atributos de cada grafico
      FirstElement(graficos())
      For g = 0 To numlocalidades - 1
        *puntero = trimspace(*puntero)
        *puntero = getpalabra(*puntero, @leida,",")
        *puntero + 1  ;avanzamos la coma
        If isnumber(leida\s)
          graficos()\atributo = Val(leida\s)
          NextElement(graficos())
        Else
          FinError("Missing number")
        EndIf
      Next
      ;y ahora leemos los datos de cada grafico, para incorporarlos
      FirstElement(graficos())
      For g = 0 To numlocalidades - 1
        For i = 1 To graficos()\tamano
          *puntero = trimspace(*puntero)
          *puntero = getpalabra(*puntero, @leida,",")
          If isnumber(leida\s)
            AddElement(graficos()\bytecoded())
            graficos()\bytecoded() = Val(leida\s) ;incorporamos el dato del grafico
            *puntero + 1                          ;avanza la ,
          Else
            FinError("Missing a number. Graphics data lost on graphic:" + StrU(g))
          EndIf
        Next i
        NextElement(graficos())
      Next g
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero, @leida,"")
      If leida\s <> "}"
        FinError("Missing } on graphics")
      EndIf
    EndIf 
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure rellenagraficos(List graficos.SGrafico(), numlocalidades, atributo)
  Shared verbose
  added = 0
  LastElement(graficos())
  While ListSize(graficos()) < numlocalidades
    added = 1
    AddElement(graficos())
    graficos()\tamano = 1
    graficos()\atributo = atributo
    AddElement(graficos()\bytecoded())
    graficos()\bytecoded() = 7
  Wend
  While ListSize(graficos()) > numlocalidades
    added = -1
    DeleteElement(graficos())
  Wend
  If verbose
    If added = 1
      PrintN("[!] Empty graphics added to all locations")
    ElseIf added = -1
      PrintN("[!] Discarded some graphics to crop to existing locations")
    EndIf
  EndIf
EndProcedure

Procedure CargaCaracteres (*puntero, List udg(), List fonts())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Characters")
  Else
    For g = 0 To 279
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,",")  ;leemos el numero del localidades presentes
      *puntero + 1                                ;avanzamos la coma
      If isnumber(leida\s)
        AddElement(udg())
        udg() = Val(leida\s)
      Else
        FinError("Missing number on Characters")
      EndIf
    Next
    *puntero = trimspace(*puntero)
    *puntero = getpalabra(*puntero,@leida,",")  ;leemos el numero del fonts
    *puntero +1 ;avanzamos la coma si la hay, y si no será el espacio o retorno de carro
    If isnumber(leida\s)
      numfonts = Val(leida\s)
    Else
      FinError("Missing number on Characters")
    EndIf
    For g = 0 To (numfonts*768) - 1
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero, @leida,",")
      *puntero + 1 ;avanzamos la coma
      If isnumber(leida\s)
        AddElement(fonts())
        fonts() = Val(leida\s)
      Else
        FinError("Missing number on characters")
      EndIf
    Next
    *puntero = trimspace(*puntero)
    *puntero = getpalabra(*puntero, @leida,"")
    If leida\s <> "}"
      FinError("Missing } on Characters")
    EndIf
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure CargaEntradas(*puntero, List entrada.SEntrada())
  Shared condactos_defs.SCondacto()
  *memoria_temporal = AllocateMemory(16000)
  leida.String
  Repeat
    *puntero = trimspace(*puntero)
    *puntero = getpalabra(*puntero,@leida,"")
    If leida\s = "}"
      Break
    ElseIf isnumber(leida\s)
      AddElement(entrada())
      entrada()\verbo = Val(leida\s)
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,":")
      *puntero + 1  ;para avanzar los :
      If isnumber(leida\s)
        entrada()\nombre = Val(leida\s)
        entrada()\key_para_ordenar = RSet(StrU(entrada()\verbo), 3, "0") + " " + RSet(StrU(entrada()\nombre), 3, "0")
        Repeat
        *puntero = trimspace(*puntero)
        *puntero =getpalabra(*puntero, @leida,"")
        If leida\s = ";"
          entrada()\hash = SHA1Fingerprint(*memoria_temporal, ListSize(entrada()\condactos()))
          Break
        ElseIf isnumber(leida\s)
          AddElement(entrada()\condactos())
          entrada()\condactos()=Val(leida\s)
          PokeA(*memoria_temporal -1 + ListSize(entrada()\condactos()), Val(leida\s))
        Else
          ;busca el condacto en la tabla, y si está lo mete, y si no está canta error
          If FindMapElement(condactos_defs(),leida\s)
            AddElement(entrada()\condactos())
            entrada()\condactos()=condactos_defs()\numero
            PokeA(*memoria_temporal -1 + ListSize(entrada()\condactos()), condactos_defs()\numero)
          Else
            FinError("Wrong sintaxis on an entry: " + StrU(entrada()\verbo)+" "+StrU(entrada()\nombre)+". Condact invalid: " + leida\s)
          EndIf
        EndIf
        ForEver
      Else
        FinError("Missing a noun number on an entry")
      EndIf
    Else
      FinError("Missing a verb number on an entry")
    EndIf
  ForEver
  FreeMemory(*memoria_temporal)
  ProcedureReturn *puntero
EndProcedure

Procedure CargaProcesos (*puntero, List procesos.SProceso())
  *puntero=trimspace(*puntero)
  leida.String
  *puntero = getpalabra(*puntero, @leida,"")
  If leida\s <> "{"
    FinError("Missing { on Process")
  Else
    Repeat
      *puntero = trimspace(*puntero)
      *puntero = getpalabra(*puntero,@leida,"")
      Select leida\s
        Case "PROCESS"
          *puntero = trimspace(*puntero)
          *puntero = getpalabra(*puntero,@leida,"")
          If isnumber(leida\s)
            AddElement(procesos())
            procesos()\numero = Val(leida\s)
            *puntero = trimspace(*puntero)
            *puntero = getpalabra(*puntero,@leida,"")
            If leida\s = "{"
              *puntero = CargaEntradas(*puntero,procesos()\entrada())
            Else
              FinError("Wrong sintaxis on PROCESS sentence")
            EndIf
          Else
            FinError("Missing number after PROCESS sentence")
          EndIf
        Case "}"
          Break
        Default
          FinError("Wrong sintaxis on Process List")
      EndSelect
    ForEver
  EndIf
  ProcedureReturn *puntero
EndProcedure

Procedure GetTextSizeToDump(Map textos.STexto(), primero, ultimo) ;hace el sumatorio del tamaño de los textos y calcula el tamaño del índice de punteros
  tamanno = 0
  For g = primero To ultimo
    If FindMapElement(textos(), StrU(g))
      tamanno + textos()\tamano
    Else
      FinError("[*] Text missing to find size")
    EndIf
  Next
  ProcedureReturn tamanno + 2*(ultimo - primero + 1)
EndProcedure

Procedure GetTamanoGraficos(List graficos.SGrafico(), primero, ultimo)
  ;CALCULO TAMAÑO DE LOS GRAFICOS
  ;es del modo siguiente
  ;El TOP de los graficos está en $FFD4
  ;debajo hay 1 byte de atributos por cada localidad  : numlocalidades
  ;$FFFF
  ;ptr_base_indice
  ;ptr_datos_localidadN-1
  ;ptr_datos_localidadN-2
  ;..
  ;ptr_datos_localidad_0    ;(numlocalidades + 2) * 2 ;el +2 es por los dos punteros superiores (ptr_base_indice y FFFF)
  ;datos_localidadN-1       ;sumatorio de todos los tamaños de los graficos
  ;datos_localidadN-2
  ;..
  ;datos_localidad0
  num_to_calculate = ultimo - primero + 1
  tamanno = num_to_calculate +  2 * (num_to_calculate + 2)
  For g=primero To ultimo
    SelectElement(graficos(), g)
    tamanno + graficos()\tamano
  Next g
  ProcedureReturn tamanno
EndProcedure

Procedure GetTamanoLocalidades(Map textos.STexto(), Map conexiones.SConexion(), primero, ultimo)
  ;ahora hay que añadir también las conexiones de las localidades
  ;las conexiones se dumpean del modo siguiente
  ;verbo destino
  ;verbo2 destino2
  ;..
  ;verbon destinon
  ;255
  ;esto por cada localidad. Y después un índice de punteros por cada localidad hacia sus conexiones
  tamanno = GetTextSizeToDump(textos(), primero, ultimo)
  For g = primero To ultimo
    If FindMapElement(conexiones(),StrU(g))
      tamanno = tamanno + 2*ListSize(conexiones()\enlace()) ;cada enlace en una localidad son 2 bytes
      tamanno + 1                                           ;y el terminador
    Else
      tamanno + 1 ;si no hay conexiones en una localidad se cuenta el finalizador
    EndIf
  Next g
  ;ahora le añado los índices
  ProcedureReturn tamanno + 2*(ultimo - primero + 1)
EndProcedure

Procedure DumpTexto (pagina, offsettodump, Map textos.STexto(), primero, ultimo, storeindice, counterpagger)
  ;counterpagger es un dato que se pone en todas las paginas de RAM
  ;indica cuantos elementos de este tipo tiene volcados en esa pagina
  NewList indices()
  For g = primero To ultimo
    If FindMapElement(textos(), StrU(g))
      AddElement(indices())
      indices() = offsettodump
      For i = 0 To textos()\tamano - 1
        poke(pagina, offsettodump, PeekA(textos()\mensaje + i) ! $ff) ;los mensajes se hacen dump con complemento a 1
        offsettodump + 1
      Next i
    Else
      FinError("Couldn't find text reference: " + StrU(g))
    EndIf
  Next g
  poke_w(pagina, storeindice, offsettodump) ;guardamos el puntero al indice
  ForEach indices()
    poke_w(pagina, offsettodump, indices())
    offsettodump + 2
  Next
  FreeList(indices())
  poke(pagina, counterpagger, ultimo - primero + 1)
  ProcedureReturn offsettodump
EndProcedure

Procedure DumpProcesos(pagina, offsettodump, List procesos.SProceso())
  ;La estructura del volcado es la siguiente:
  ;
  ;Proceso 0...
  ;
  ;condactos_entrada_0,,, 255
  ;condactos_entrada_1,,, 255
  ;condactos_entrada_n,,, 255
  ;255
  ;verbo_entrada_0
  ;nombre_entrada_0
  ;puntero_a_condactos_entrada_0
  ;verbo_entrada_1
  ;nombre_entrada_1
  ;puntero_a_condactos_entrada_1
  ;...
  ;0
  ;0
  ;puntero_al_255_final
  ;
  ;gracias a los hashes se puede saber si los condactos de una entrada ya estan volcados, y así evitar repetir entradas
  ;ya que la tabla de verbo y nombre apuntará a un mismo bloque de condactos
  
  ;primero ordenamos los procesos por número
  ;y las entradas de cada proceso por verbo - nombre
  
  NewMap offsets_volcadas() ;genero un mapa de los hashes de los condactos y el offset donde están volcados
  NewList indices_entradas.SEntradaOffset() ;las entradas se han de ir generando con verbo-nombre-offset_de_los_condactos
  NewList offset_procesos()                 ;cada proceso tiene un puntero que apunta al índice de las entradas
  
  SortStructuredList(procesos(), #PB_Sort_Ascending, OffsetOf(SProceso\numero), TypeOf(SProceso\numero))
  ForEach procesos()
    If verbose = 2: PrintN("[-] Offset of process " + StrU(procesos()\numero) + " condacts: "+ Hex(offsettodump,#PB_Word)): EndIf
    ;HE QUITADO LA ORDENACION, PORQUE Inpaws ES QUIEN LO ORDENA POR SI MISMO
    ;yo segun los he leido del preprocesado los meto
    ;
    SortStructuredList(procesos()\entrada(),#PB_Sort_Ascending, OffsetOf(SEntrada\key_para_ordenar), TypeOf(SEntrada\key_para_ordenar))
    ClearList(indices_entradas())
    ForEach procesos()\entrada()
      If Not FindMapElement(offsets_volcadas(), procesos()\entrada()\hash)
        ;si aun no está volcado, se vuelca y se registra como tal
        offsetentrada = offsettodump
        offsets_volcadas(procesos()\entrada()\hash) = offsetentrada
        ForEach procesos()\entrada()\condactos()
          poke(pagina, offsettodump, procesos()\entrada()\condactos())
          offsettodump + 1
          If offsettodump > 65535 : FinError("[!] Process size too large"):EndIf
        Next
        ;después de todos los condactos de una entrada se termina con 255
        poke(pagina,offsettodump,255)
        offsettodump + 1
      Else
        ;si ya está volcado, no se vuelve a volcar, y se coge el offset del que ya está volcado
        offsetentrada = offsets_volcadas()
      EndIf
      ;lo añadimos a la lista de entradas
      AddElement(indices_entradas())
      indices_entradas()\verbo = procesos()\entrada()\verbo
      indices_entradas()\nombre = procesos()\entrada()\nombre
      indices_entradas()\offset = offsetentrada
    Next
    poke(pagina, offsettodump, 255)
    AddElement(indices_entradas())
    indices_entradas()\verbo = 0
    indices_entradas()\nombre = 0
    indices_entradas()\offset = offsettodump
    offsettodump + 1
    ;tras haber volcado todos los condactos de las entradas
    ;ahora se vuelca el índice de las entradas (nombre, verbo, y offset)
    ;el terminador ya está incluido
    AddElement(offset_procesos())
    If verbose = 2: PrintN("[-] Offset of process " + StrU(procesos()\numero) + " entries index: "+ Hex(offsettodump,#PB_Word)): EndIf
    offset_procesos() = offsettodump
    ForEach indices_entradas()
      If offsettodump > 65531 : FinError("[!] Process size too large"):EndIf
      poke(pagina, offsettodump, indices_entradas()\verbo)
      poke(pagina, offsettodump + 1, indices_entradas()\nombre)
      poke_w(pagina, offsettodump + 2, indices_entradas()\offset)
      offsettodump + 4
    Next
  Next
  ;finalmente volcamos todos los punteros de cada proceso a su tabla de indices
  ;;esta tabla es la que hay que marcar como offset de los procesos
  ;
  poke_w(pagina,$ffd9,offsettodump) ;65497
  If verbose = 2: PrintN("[-] Offset of all process Index:" + Hex(offsettodump,#PB_Word)): EndIf
  ForEach offset_procesos()
    If offsettodump > 65533 : FinError("[!] Process size too large"):EndIf
    poke_w(pagina,offsettodump,offset_procesos())
    offsettodump + 2
  Next
  poke(pagina,$fffc, ListSize(procesos()))
  ProcedureReturn offsettodump
EndProcedure

Procedure DumpConexiones(pagina, offsettodump, Map conexiones.SConexion(), primero, ultimo, storeindice)
;las conexiones se dumpean del modo siguiente
;verbo destino
;verbo2 destino2
;..
;verbon destinon
;255
;esto por cada localidad. Y después un índice de punteros por cada localidad hacia sus conexiones
  NewList indices()
  For g = primero To ultimo
    AddElement(indices())
    indices() = offsettodump
    If FindMapElement(conexiones(), StrU(g))
      ForEach conexiones()\enlace()
        poke(pagina, offsettodump, conexiones()\enlace()\verbo)
        poke(pagina, offsettodump + 1, conexiones()\enlace()\destino)
        offsettodump + 2
      Next
    EndIf
    poke(pagina, offsettodump, $ff)
    offsettodump + 1
  Next g
  poke_w(pagina, storeindice, offsettodump)
  ForEach indices()
    poke_w(pagina, offsettodump, indices())
    offsettodump + 2
  Next
  ProcedureReturn offsettodump
EndProcedure

Procedure DumpVocabulary(pagina, puntero_datos, List vocabulario.SVocablo(), storebase)
  poke_w(pagina, storebase, puntero_datos)
  SortStructuredList(vocabulario(), #PB_Sort_Ascending, OffsetOf(SVocablo\numero), TypeOf(SVocablo\numero))
  ForEach vocabulario()
    For g = 1 To 5
      poke(pagina, puntero_datos, Asc(Mid(vocabulario()\palabra,g,1)) ! $ff)  ;se guarda complementada
      puntero_datos + 1
    Next
    poke(pagina, puntero_datos, vocabulario()\numero) ;numero id de la palabra
    poke(pagina, puntero_datos + 1, vocabulario()\tipo) ;tipo de palabra
    puntero_datos + 2
  Next
  ;terminado del vocabulario : 7veces 0
  For g = 1 To 7
    poke(pagina, puntero_datos, 0)
    puntero_datos + 1
  Next g
  ProcedureReturn puntero_datos
EndProcedure

Procedure DumpInitiallyat(pagina, puntero_datos, Map objetos_inicial.SObjeto(), primero, ultimo, storebase)
  poke_w(pagina, storebase, puntero_datos)
  For g = primero To ultimo
    If FindMapElement(objetos_inicial(), StrU(g))
      poke(pagina, puntero_datos, objetos_inicial()\inicialmente)
    Else
      poke(pagina, puntero_datos, 252)  ;si no está definido el objeto, lo dejamos como no creado
    EndIf
    puntero_datos + 1
  Next g
  poke(pagina, puntero_datos, 255)  ;terminador
  ProcedureReturn puntero_datos + 1
EndProcedure

Procedure DumpObjPalabras(pagina, puntero_datos, Map objetos_inicial.SObjeto(), primero, ultimo, storebase)
  poke_w(pagina, storebase, puntero_datos)
  For g = primero To ultimo
    If FindMapElement(objetos_inicial(),StrU(g))
      poke(pagina, puntero_datos, objetos_inicial()\nombre)
      poke(pagina, puntero_datos + 1, objetos_inicial()\adjetivo)
    Else
      ;si no existe el objeto creado, se pone como _ _ en nombre y adjetivo
      poke(pagina, puntero_datos, 255)
      poke(pagina, puntero_datos + 1, 255)
    EndIf
    puntero_datos + 2
  Next
  ;terminadores
  poke(pagina, puntero_datos, 0)
  poke(pagina, puntero_datos + 1, 0)
  ProcedureReturn puntero_datos + 2
EndProcedure

Procedure DumpObjPesos(pagina, puntero_datos, Map objetos_inicial.SObjeto(), primero, ultimo, storebase)
  poke_w(pagina, storebase, puntero_datos)
  For g = primero To ultimo
    If FindMapElement(objetos_inicial(),StrU(g))
      poke(pagina, puntero_datos, objetos_inicial()\peso)
    Else
      ;si no existe el objeto creado, se pone 1 como peso
      poke(pagina, puntero_datos, 1)
    EndIf
    puntero_datos + 1
  Next
  ;los pesos de los objetos no tienen terminadores
  ProcedureReturn puntero_datos
EndProcedure

Procedure DumpGraficos(pagina, offsettodump, List graficos.SGrafico(), primero, ultimo, storeindice, storeatributos)
  NewList indices()
  poke_w(pagina, $ffef, offsettodump)
  For g = primero To ultimo
    AddElement(indices())
    indices() = offsettodump
    If SelectElement(graficos(), g)
      ForEach graficos()\bytecoded()
        poke(pagina, offsettodump, graficos()\bytecoded())
        offsettodump + 1
      Next
    Else
      FinError("[!] Trying to access a graphics that doesn't exist")
    EndIf
  Next g
  ;se añade como ultimo elemento de los índices: la base de los índices, y FFFF
  AddElement(indices())
  indices() = offsettodump
  AddElement(indices())
  indices() = $ffff
  poke_w(pagina, storeindice, offsettodump)
  ;volcamos los índices
  ForEach indices()
    poke_w(pagina, offsettodump, indices())
    offsettodump + 2
  Next
  ;y ahora volcamos los atributos de los graficos
  ;
  poke_w(pagina, storeatributos, offsettodump)
  For g = primero To ultimo
    SelectElement(graficos(), g)
    poke(pagina,offsettodump,graficos()\atributo)
    offsettodump + 1
  Next
  FreeList(indices())
  poke(pagina, offsettodump, 255) ;este marcador va siempre, y debe quedar en $ffd4
  poke_w(pagina, $fff5, offsettodump) ;este puntero se situa siempre en todas las BD así
  ProcedureReturn offsettodump
  
EndProcedure

Procedure DumpPaginacion(puntero_paginas)
  Shared paginas_localidades()
  Shared paginas_mensajes()
  g = 0
  While g < 8
    poke(0, puntero_paginas, paginas_mensajes(g))
    poke(0,puntero_paginas + 1, g)
    puntero_paginas + 2
    g = nextpagina(g)
  Wend
  ;se añade un terminador
  poke_w(0, puntero_paginas, $ffff)
  puntero_paginas + 2
  g = 0
  While g < 8
    poke(0, puntero_paginas, paginas_localidades(g))
    poke(0,puntero_paginas + 1, g)
    puntero_paginas + 2
    g = nextpagina(g)
  Wend
  ;se añade el terminador
  poke_w(0,puntero_paginas, $ffff)
EndProcedure

Procedure GrabaBloqueEnTap (archivo, pagina, inicio, fin, nombre_spectrum$)
  Shared memoria()
  ;cabecera
  WriteWord(archivo,19) ;tamaño de la cabecera
  WriteAsciiCharacter(archivo, 0) ;flag de la cabecera
  WriteAsciiCharacter(archivo, 3) ;tipo de bloque: code
  crc = 3
  For g = 1 To 10
    WriteAsciiCharacter(archivo, Asc(Mid(nombre_spectrum$,g,1)))
    crc = crc ! Asc(Mid(nombre_spectrum$, g, 1))
  Next g
  tamano_bloque = fin - inicio + 1
  WriteWord(archivo, tamano_bloque)
  crc = crc ! low_byte(tamano_bloque) ! hight_byte(tamano_bloque)
  WriteWord(archivo,inicio)
  crc = crc ! low_byte(inicio) ! hight_byte(inicio)
  WriteWord(archivo, $8000)
  crc = crc ! $80
  WriteAsciiCharacter(archivo, crc)
  
  WriteWord(archivo, tamano_bloque + 2) ;van los datos, mas el flag y el crc
  WriteAsciiCharacter(archivo, 255)     ;flag de datos
  crc = 255
  For g = inicio To fin
    WriteAsciiCharacter(archivo, memoria(pagina,g))
    crc = crc ! memoria(pagina, g)
  Next g
  WriteAsciiCharacter(archivo, crc)
EndProcedure

Procedure GetFreePointer(puntero_inicial, locinicial, locfinal, mensinicial, mensfinal, *datos.SComunesToDump)
tamanno_objetos_texto = GetTextSizeToDump(*datos\objetos(), 0, MapSize(*datos\objetos()) - 1)
tamanno_objetos = (MapSize(*datos\objetos_inicial()) + 1) * 3 + MapSize(*datos\objetos_inicial())
tamanno_vocabulario = (ListSize(*datos\vocabulario()) + 1) * 7
tamanno_sysmensajes = GetTextSizeToDump(*datos\sysmensajes(), 0, MapSize(*datos\sysmensajes()) - 1)
tamanno_mensajes = GetTextSizeToDump(*datos\mensajes(), mensinicial, mensfinal)
tamanno_localidades = GetTamanoLocalidades(*datos\localidades(), *datos\conexiones(), locinicial, locfinal)
fin_calculado = puntero_inicial + tamanno_objetos_texto + tamanno_objetos
fin_calculado = fin_calculado + tamanno_vocabulario + tamanno_sysmensajes + tamanno_mensajes + tamanno_localidades
ProcedureReturn fin_calculado
EndProcedure

Procedure GetStartGrpahics(List graficos.SGrafico(), inicio, fin)
  ProcedureReturn $ffd4 - GetTamanoGraficos(graficos(), inicio, fin)
EndProcedure

Procedure IsPawsDBPresent()
  If peek_w(0,37942) = 4096 And peek(0,37912) = 20
    pagina = 0
    posicion = 37915
    valid = #True
    While pagina < 8
      If peek(0,posicion) = 255 Or peek(0,posicion) = 255
        Break
      EndIf
      If peek(0, posicion) <> pagina Or peek(0, posicion+14) <> pagina
        valid = #False
      EndIf
      pagina = nextpagina(pagina)
      posicion + 2
    Wend
    ProcedureReturn valid
  Else
    ProcedureReturn #False
  EndIf
EndProcedure

Procedure AbreviaturasDesdeMemoria(Map abreviaturas.String(), puntero)
  ClearMap(abreviaturas())
  If peek(0, puntero) <> 255
    For g=164 To 254
      cadena$=""
      endcaracter = #False
      Repeat
        caracter = peek(0,puntero)
        If caracter > 127 : caracter - 128: endcaracter = #True : EndIf
        If caracter < 32
          cadena$ = cadena$ + "{" + StrU(caracter) + "}"
        Else
          cadena$ + Chr(caracter)
        EndIf
        puntero + 1
      Until endcaracter
      abreviaturas("{"+StrU(g)+"}")\s = cadena$
    Next g
  EndIf
EndProcedure

Procedure VocabularioDesdeMemoria(List vocabulario.SVocablo(), puntero)
  ;el vocabulario siempre está en la pagina 0
  While peek_w(0,puntero) <> 0 And peek_w(0,puntero + 2) <> 0
    palabra$=""
    For g=0 To 4
      palabra$ = palabra$ + Chr(peek(0, puntero + g) ! $ff)
    Next
    AddElement(vocabulario())
    With vocabulario()
      \palabra = RTrim(palabra$)
      \numero = peek(0, puntero + 5)
      \tipo = peek(0, puntero + 6)
      \keytosort = RSet(Str(\numero),3,"0") + RSet(Str(\tipo), 3, "0") + \palabra
    EndWith
    puntero + 7
  Wend
  SortStructuredList(vocabulario(),#PB_Sort_Ascending, OffsetOf(SVocablo\keytosort), TypeOf(SVocablo\keytosort))
EndProcedure

Procedure VocabularioToSource(archivo, List vocabulario.SVocablo())
  SortStructuredList(vocabulario(),#PB_Sort_Ascending, OffsetOf(SVocablo\keytosort), TypeOf(SVocablo\keytosort))
  WriteStringN(archivo, "VOCABULARY")
  WriteStringN(archivo, "{")
  numero = 0
  tipo = 255
  ForEach vocabulario()
    If vocabulario()\numero <> numero Or vocabulario()\tipo <> tipo
      Select vocabulario()\tipo
        Case 0
          tipostring$="VERB"
        Case 1
          tipostring$="ADVERB"
        Case 2
          tipostring$="NOUN"
        Case 3
          tipostring$="ADJECTIVE"
        Case 4
          tipostring$="PREPOSITION"
        Case 5
          tipostring$="CONJUNCTION"
        Case 6
          tipostring$="PRONOUN"
      EndSelect
      If tipo <> 255 And vocabulario()\tipo <> 255
        WriteStringN(archivo, ";")
      EndIf
      If vocabulario()\tipo <> 255
        WriteString(archivo, Chr(9) + tipostring$ + " " + StrU(vocabulario()\numero) + ": ")
        WriteString(archivo, Chr(34) + vocabulario()\palabra + Chr(34))
      EndIf
    Else
      WriteString(archivo, ", " + Chr(34) + vocabulario()\palabra + Chr(34))
    EndIf
    tipo = vocabulario()\tipo
    numero = vocabulario()\numero
  Next
  WriteStringN(archivo, ";")
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
EndProcedure

Procedure DefaultsToSource(archivo)
  WriteStringN(archivo, "DEFAULTS")
  WriteStringN(archivo, "{")
  WriteStringN(archivo, Chr(9) + "CHARSET: " + StrU(peek(0, 37913)) + ";")
  WriteStringN(archivo, Chr(9) + "INK: " + StrU(peek(0, 37944)) + ";")
  WriteStringN(archivo, Chr(9) + "PAPER: " + StrU(peek(0, 37946)) + ";")
  WriteStringN(archivo, Chr(9) + "FLASH: " + StrU(peek(0, 37948)) + ";")
  WriteStringN(archivo, Chr(9) + "BRIGHT: " + StrU(peek(0, 37950)) + ";")
  WriteStringN(archivo, Chr(9) + "INVERSE: " + StrU(peek(0, 37952)) + ";")
  WriteStringN(archivo, Chr(9) + "OVER: " + StrU(peek(0, 37954)) + ";")
  WriteStringN(archivo, Chr(9) + "BORDER: " + StrU(peek(0, 37955)) + ";")
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
EndProcedure

Procedure.s StringDecode(pagina, ptr, Map abreviaturas.String())
  string$=""
  fincadena=#False
  previo = 0
  Repeat
    caracter = peek(pagina, ptr) ! $ff
    Select caracter
      Case 7
        If previo <> 16 And previo<>17
          string$ = string$ + "^"
        Else
          string$ = string$ + "{"+StrU(caracter)+"}"
        EndIf
      Case 34, Asc("\"), Asc("^"), Asc("{")
        string$ = string$ + "\" + Chr(caracter)  
      Case 31
        fincadena = #True
      Case 31 To 128
        string$ + Chr(caracter)
      Case 0 To 30, 128 To 255
        string$ = string$ + "{"+StrU(caracter)+"}"
    EndSelect
    previo = caracter
    ptr + 1
  Until fincadena
  ForEach abreviaturas()
    string$ = ReplaceString(string$, MapKey(abreviaturas()), abreviaturas()\s)
  Next
  ProcedureReturn string$
EndProcedure

Procedure LocationsToSource(archivo, cantidad, Map abreviaturas.String(), List vocabulario.SVocablo())
  actual = 0
  paginas_ptr = 37927
  Repeat
    If actual = memoria(0, paginas_ptr + 1) ;si se alcanza el máximo de esta página, hay que avanzar y actualizar puntero de índices
      paginas_ptr + 2
      pagina = peek(0,paginas_ptr)
      base = peek(0,paginas_ptr - 1)
      index_ptr = peek_w(pagina, $ffdd)
      con_index_ptr = peek_w(pagina, $ffe3)
    EndIf
    WriteStringN(archivo, "LOCATION "+StrU(actual))
    WriteStringN(archivo, "{")
    WriteStringN(archivo, Chr(9) + Chr(34) + StringDecode(pagina,peek_w(pagina,index_ptr), abreviaturas()) + Chr(34) + ";")
    offset_con = peek_w(pagina, con_index_ptr)
    If peek(pagina, offset_con) <> 255
      WriteString(archivo, Chr(9) + "CONNECTIONS { ")
      Repeat
        verbo = peek(pagina, offset_con)
        destino = peek(pagina, offset_con + 1)
        ForEach vocabulario()
          If verbo = vocabulario()\numero And (vocabulario()\tipo = 2  Or vocabulario()\tipo = 0) ;que coincida en numero y tipo (las conexiones son de tipo NOUN o  VERB)
            WriteString(archivo, vocabulario()\palabra + " TO " + StrU(destino) + " ")
            Break
          EndIf
        Next
        offset_con + 2
      Until peek(pagina, offset_con) = 255
      WriteStringN(archivo, "};")
    EndIf
    index_ptr + 2
    con_index_ptr + 2
    actual + 1
    WriteStringN(archivo, "}")
    WriteStringN(archivo, "")
  Until actual = cantidad Or actual = 255
EndProcedure

Procedure MessagesToSource(archivo, cantidad, Map abreviaturas.String())
  actual = 0
  paginas_ptr = 37913
  WriteStringN(archivo, "MESSAGES")
  WriteStringN(archivo, "{")
  Repeat
    If actual = memoria(0, paginas_ptr + 1) ;si se alcanza el máximo de esta página, hay que avanzar y actualizar puntero de índices
      paginas_ptr + 2
      pagina = peek(0,paginas_ptr)
      base = peek(0,paginas_ptr - 1)
      index_ptr = peek_w(pagina, $ffdf)
    EndIf
    WriteStringN(archivo, Chr(9) + StrU(actual)+ ": " + Chr(34) + StringDecode(pagina,peek_w(pagina,index_ptr), abreviaturas()) + Chr(34) + ";")
    index_ptr + 2
    actual + 1
  Until actual = cantidad Or actual = 255
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
EndProcedure

Procedure SysmessagesToSource(archivo, cantidad, Map abreviaturas.String())
  WriteStringN(archivo, "SYSMESSAGES")
  WriteStringN(archivo, "{")
  index_ptr = peek_w(0, $ffe1)
  For g = 0 To cantidad - 1
    WriteStringN(archivo, Chr(9) + StrU(g)+ ": " + Chr(34) + StringDecode(0,peek_w(0,index_ptr), abreviaturas()) + Chr(34) + ";")
    index_ptr + 2
  Next g
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
EndProcedure

Procedure CharactersToSource(archivo, numerofuentes)
  WriteStringN(archivo, "CHARACTERS")
  WriteStringN(archivo, "{")
  coma = #False
  For g = 0 To 279
    If coma
      WriteString(archivo,", ")
    EndIf
    If g % 8 = 0
      WriteStringN(archivo,"")
      WriteString(archivo, Chr(9))
    EndIf
    WriteString(archivo, StrU(peek(0,37632 + g)))
    coma = #True
  Next g
  WriteStringN(archivo,",")
  WriteString(archivo, Chr(9) + StrU(numerofuentes))
  offsetfonts = peek_w(0, 37962)
  For g = 0 To (numerofuentes * 768) -1
    WriteString(archivo,", ")
    If g % 8 = 0
      WriteStringN(archivo,"")
      WriteString(archivo, Chr(9))
    EndIf
    WriteString(archivo, StrU(peek(0, offsetfonts + g)))
  Next g
  WriteStringN(archivo, "")
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
EndProcedure

Procedure ObjetosToSource(archivo, cantidad, Map abreviaturas.String(), List vocabulario.SVocablo())
  offset_index_textos = peek_w(0, $ffdb)
  offset_initially = peek_w(0, $ffe7)
  offset_words = peek_w(0, $ffe9)
  offset_weight = peek_w(0, $ffeb)
  
  For g = 0 To cantidad - 1
    WriteStringN(archivo, "OBJECT " + StrU(g))
    WriteStringN(archivo, "{")
    WriteStringN(archivo, Chr(9) + Chr(34) + StringDecode(0, peek_w(0, offset_index_textos), abreviaturas()) + Chr(34)+";")
    inicialmente = peek(0, offset_initially)
    Select inicialmente
      Case 254
        place$ = "CARRIED"
      Case 253
        place$ = "WORN"
      Case 252
        place$ = "NOTCREATED"
      Default
        place$ = StrU(inicialmente)
    EndSelect
    WriteStringN(archivo, Chr(9) + "INITIALLYAT " + place$ + ";")
    nombre = peek(0, offset_words)
    adjetivo = peek(0, offset_words + 1)
    If nombre = 255
      nombre_string$ = "_"
    Else
      ForEach vocabulario()
        If vocabulario()\numero = nombre And vocabulario()\tipo = 2
          Break
        EndIf
      Next
      nombre_string$=vocabulario()\palabra
    EndIf
    If adjetivo = 255
      adjetivo_string$ = "_"
    Else
      ForEach vocabulario()
        If vocabulario()\numero = adjetivo And vocabulario()\tipo = 3
          Break
        EndIf
      Next
      adjetivo_string$=vocabulario()\palabra
    EndIf
    WriteStringN(archivo, Chr(9) + "WORDS " + nombre_string$ + " " + adjetivo_string$ +";")
    peso = peek(0, offset_weight)
    WriteStringN(archivo, Chr(9) + "WEIGHT " + StrU(peso & $3F) + ";")
    If peso > 63
      WriteString(archivo, Chr(9) + "PROPERTY ")
      If (peso & $80) > 0
        WriteString(archivo, "CLOTHING")
      EndIf
      If (peso & $c0) = $c0 ;si están activadas las 2, hay que poner una coma
        WriteString(archivo, ", ")
      EndIf
      If (peso & $40) > 0
        WriteString(archivo, "CONTAINER")
      EndIf
      WriteStringN(archivo, ";")
    EndIf
    offset_index_textos + 2
    offset_initially + 1
    offset_words + 2
    offset_weight + 1
    WriteStringN(archivo, "}")
  Next g
  WriteStringN(archivo, "")
EndProcedure

Procedure GraphicsToSource(archivo, cantidad)
  actual = 0
  paginas_ptr = 37927
  ;primero generamos los graficos en una lista interna
  NewList graficos_from_memory.SGrafico()
  Repeat
    If actual = memoria(0, paginas_ptr + 1) ;si se alcanza el máximo de esta página, hay que avanzar y actualizar puntero de índices
      paginas_ptr + 2
      pagina = peek(0,paginas_ptr)
      base = peek(0,paginas_ptr - 1)
      gfx_index_ptr = peek_w(pagina, $fff1)
      gfx_attr_ptr = peek_w(pagina, $fff3)
    EndIf
    AddElement(graficos_from_memory())
    graficos_from_memory()\atributo = peek(pagina, gfx_attr_ptr)
    graficos_from_memory()\tamano = peek_w(pagina, gfx_index_ptr + 2) - peek_w(pagina, gfx_index_ptr)
    For g = 0 To graficos_from_memory()\tamano - 1
      AddElement(graficos_from_memory()\bytecoded())
      graficos_from_memory()\bytecoded()= peek_w(pagina, peek_w(pagina, gfx_index_ptr) + g)
    Next g
    actual + 1
    gfx_attr_ptr + 1
    gfx_index_ptr + 2
  Until actual = cantidad Or actual = 255
  WriteStringN(archivo, "GRAPHICS")
  WriteStringN(archivo, "{")
  If cantidad > 0
    WriteString(archivo, Chr(9)+StrU(cantidad)+",")
    count = 0
    ForEach graficos_from_memory()
      If count <> 0
        WriteString(archivo, ", ")
      EndIf
      If count % 10 = 0
        WriteStringN(archivo,"")
        WriteString(archivo, Chr(9))
      EndIf      
      WriteString(archivo, StrU(graficos_from_memory()\tamano))
      count + 1
    Next
    WriteString(archivo,",")
    count = 0
    ForEach graficos_from_memory()
      If count <> 0
        WriteString(archivo, ", ")
      EndIf
      If count % 10 = 0
        WriteStringN(archivo,"")
        WriteString(archivo, Chr(9))
      EndIf
      WriteString(archivo, StrU(graficos_from_memory()\atributo))
      count + 1
    Next
    WriteString(archivo, ",")
    count = 0
    ForEach graficos_from_memory()
      ForEach graficos_from_memory()\bytecoded()
        If count <> 0
          WriteString(archivo, ", ")
        EndIf
        If count % 10 = 0
          WriteStringN(archivo,"")
          WriteString(archivo, Chr(9))
        EndIf
      WriteString(archivo, StrU(graficos_from_memory()\bytecoded()))
      count + 1
      Next
    Next
  EndIf
  WriteStringN(archivo, "")
  WriteStringN(archivo, "}")
  WriteStringN(archivo, "")
  
EndProcedure

Procedure.s BuscaVozPorTipo(numero,tipo, List vocabulario.SVocablo())
  If numero = 255 
    ProcedureReturn "_"
  EndIf
  
  ForEach vocabulario()
    If vocabulario()\numero = numero And vocabulario()\tipo = tipo
      ProcedureReturn vocabulario()\palabra
    EndIf
  Next
EndProcedure

Procedure ProcessToSource(archivo, cantidad, List vocabulario.SVocablo(), Map condactos_defs.SCondacto())
  offset_procesos = peek_w(0, $ffd9)
  For g = 3 To cantidad - 1
    WriteStringN(archivo, "PROCESS " + StrU(g) + ";")
  Next g
  WriteStringN(archivo, "")
  For g = 0 To cantidad - 1
    If g = 0
      WriteStringN(archivo, "RESPONSE")
    Else
      WriteStringN(archivo, "PROCESS " + StrU(g))
    EndIf
    WriteStringN(archivo, "{")
    offset_entrada = peek_w(0, offset_procesos)
    While peek_w(0, offset_entrada) <> 0
      verbo = peek(0, offset_entrada)
      nombre = peek(0, offset_entrada + 1)
      verbo_string$ = ""
      ForEach vocabulario()
        ;hay que buscar verbos con ese numero, o en su defecto nombres convertibles a verbo
        If vocabulario()\numero = verbo And (vocabulario()\tipo = 0 Or vocabulario()\tipo = 255 Or (vocabulario()\tipo = 2 And vocabulario()\numero < 20))
          verbo_string$=vocabulario()\palabra
          Break
        EndIf
      Next
      nombre_string$ = ""
      ForEach vocabulario()
        If vocabulario()\numero = nombre And (vocabulario()\tipo = 2 Or vocabulario()\tipo = 255)
          nombre_string$=vocabulario()\palabra
          Break
        EndIf
      Next
      WriteString(archivo, Chr(9) + verbo_string$ + " " + nombre_string$ + ":")
      offset_condactos = peek_w(0, offset_entrada + 2)
      offset_entrada + 4
      While peek(0, offset_condactos) <> 255
        condacto = peek(0, offset_condactos)
        num_args = 0
        ForEach condactos_defs()
          If condactos_defs()\numero = condacto
            condacto_string$ = MapKey(condactos_defs())
            WriteString(archivo," " + condacto_string$)
            Break
          EndIf
        Next
        num_args = condactos_defs()\num_argumentos
        offset_condactos + 1
        For c=1 To num_args
          arg_value = peek(0, offset_condactos)
          Select condacto_string$
            Case "ADJECT1", "ADJECT2"
              voz$ = BuscaVozPorTipo(arg_value, 3, vocabulario())
            Case "NOUN2"
              voz$ = BuscaVozPorTipo(arg_value, 2, vocabulario())
            Case "PREP"
              voz$ = BuscaVozPorTipo(arg_value, 4, vocabulario())
            Case "ADVERB"
              voz$ = BuscaVozPorTipo(arg_value, 1, vocabulario())
            Default
              voz$ = StrU(arg_value)
          EndSelect
          WriteString(archivo," "+voz$)
          offset_condactos + 1
        Next
      Wend
      WriteStringN(archivo, ";")
    Wend
    WriteStringN(archivo, "}")
    WriteStringN(archivo, "")
    offset_procesos + 2
  Next g
EndProcedure

Procedure ExtractFromFile(archivo$, archivosalida$, Map condactos_defs.SCondacto(), typeextract)
  Shared verbose
  inputfile = ReadFile(#PB_Any, archivo$)
  If inputfile
    If GetExtensionPart(archivo$) = "sna"
      tiposna = IdentifySna(inputfile)
      If  tiposna <> -1
        LoadMemoriaFromSna(tiposna, inputfile)
      Else
        FinError("Wrong SNA file")
      EndIf
    Else
      If IdentifyTap(inputfile)
        LoadMemoriaFromTap(inputfile)
      Else
        FinError("Wrong TAP file")
      EndIf
    EndIf
    ;ahora identificamos la base de datos de PAWS, revisando los valores que van hardcodeados
    If IsPawsDBPresent()
      PrintN("[*] Paws database detected and maped into memory. Input file: " + in_archivo$)
      PrintN("    DB version : " + StrU(memoria(0, $fff7)))
      ;cargamos valores generales
      numobj = peek(0,37956)
      numlocs = peek(0,37957)
      nummensajes = peek(0,37958)
      numsysmensajes = peek(0,37959)
      numprocs = peek(0,37960)
      numfonts = peek(0,37961)
      extension$ = LCase(GetExtensionPart(archivosalida$))
      If extension$ = "paw" Or extension$ = "paws"
        archivosalida$ = Left(archivosalida$, Len(archivosalida$) - Len(extension$) - 1)
      EndIf
      outputfile = CreateFile(#PB_Any, archivosalida$ + ".paw")
      If outputfile
        ;lo primero de todo cargamos abreviaturas y vocabulario
        If typeextract = #C_ALL
          NewMap abreviaturas.String()
          AbreviaturasDesdeMemoria(abreviaturas(), peek_w(0, 37964))
          If MapSize(abreviaturas()) > 0 : PrintN ("[*] Abbreviations loaded"):EndIf
          NewList vocabulario.SVocablo()
          VocabularioDesdeMemoria(vocabulario(), peek_w(0, $ffe5))
        EndIf
        PrintN("[*] Dumping into file...")
        If typeextract = #C_ALL
          DefaultsToSource(outputfile)
          LocationsToSource(outputfile, numlocs, abreviaturas(), vocabulario())
          PrintN("    Locations and connections: " + StrU(numlocs))
          VocabularioToSource(outputfile, vocabulario())
          PrintN("    Vocabulary: " + StrU(ListSize(vocabulario())) + " words")
          ObjetosToSource(outputfile, numobj,abreviaturas(),vocabulario())
          PrintN("    Objects: " + StrU(numobj))
          MessagesToSource(outputfile, nummensajes, abreviaturas())
          PrintN("    Messages: " + StrU(nummensajes))
          SysmessagesToSource(outputfile, numsysmensajes, abreviaturas())
          PrintN("    System Messages: " + StrU(numsysmensajes))
        EndIf
        If typeextract = #C_GRAPHICS Or typeextract = #C_ALL Or typeextract = #C_GFX
          GraphicsToSource(outputfile, numlocs)
        EndIf
        If typeextract = #C_CHARACTERS Or typeextract = #C_ALL Or typeextract = #C_GFX
          CharactersToSource(outputfile, numfonts)
          If numfonts > 0 : PrintN("    Character Sets: " + StrU(numfonts)):EndIf
        EndIf
        If typeextract = #C_ALL
          ProcessToSource(outputfile, numprocs, vocabulario(), condactos_defs())
          PrintN("    Processes: " + StrU(numprocs))
        EndIf
        If verbose : PrintN("[*] Output file created: " + out_archivo$):EndIf
        CloseFile(outputfile)
      Else
        FinError("Couldn't create output file: " + archivosalida$)
      EndIf
    Else
      FinError("It doesn't seems to be a valid PAWS database")
    EndIf
  Else
    FinError("Couldn't open the file: " + archivo$)
  EndIf
EndProcedure


version$ = "1.3beta"

;DEBUG MODE
verbose = #False
;MODE 128
mode_smart = #True
extract = #False

main:
OpenConsole()
PrintN("InpawsPager v" + version$ + " - by Sergio Llata (aka thEpOpE)")
in_archivo$ = ""
out_archivo$ = ""
symbols$ = ""
;cargamos los parametros
Repeat
  comando$= ProgramParameter()
  Select LCase(comando$)
    Case "c"
      control = #True
      in_archivo$ = ProgramParameter()
    Case "cp"
      control = #False
      in_archivo$ = ProgramParameter()
    Case "e"
      extract = #True
      typeextract = #C_ALL
      in_archivo$ = ProgramParameter()
    Case "ec"
      extract = #True
      typeextract = #C_CHARACTERS
      in_archivo$ = ProgramParameter()
    Case "eg"
      extract = #True
      typeextract = #C_GRAPHICS
      in_archivo$ = ProgramParameter()
    Case "egfx"
      extract = #True
      typeextract = #C_GFX
      in_archivo$ = ProgramParameter()
    Case "-o"
      out_archivo$ = ProgramParameter()
    Case "-m"
      modo$= LCase(ProgramParameter())
      If  modo$ = "structured"
        mode_smart = #False
      ElseIf modo$ = "smart"
        mode_smart = #True
      Else
        FinError("Wrong mode")
      EndIf
    Case "-v"
      verbose = #True
    Case "-s"
      symbols$ = " -s "
    Case "-dbg"
      verbose = 2
    Case "--help","-h"
      PrintN("Commands:")
      PrintN("   c <inpunt_file.paw> : compiles input_file.paw using Inpaws, and pack into a 48/128 Tap")
      PrintN("   cp <inpunt_file.ppr> : compiles a input_file.ppr preprocessed by Inpaws, and pack into a 48/128 Tap")
      PrintN("   e <input_file.tap|sna> : extracts the DataBase from an snapshot or Tap file and dumps into Inpaws sintaxis")
      PrintN("   ec <input_file.tap|sna> : extracts the Udg, Shades and Sets of characters from an snapshot or Tap file and dumps into Inpaws sintaxis")
      PrintN("   eg <input_file.tap|sna> : extracts the graphics and default colors of locations from an snapshot or Tap file and dumps into Inpaws sintaxis")
      PrintN("   egfx <input_file.tap|sna> : extracts the Udg, Shades, Sets of characters, graphics and default colors from an snapshot or Tap file and dumps into Inpaws sintaxis")
      PrintN("")
      PrintN("Options:")
      PrintN("   -o <output_file.tap> : selects a file for output the final tap")
      PrintN("   -m smart | structured : selects the way the final tap is organized. Smart is the default mode")
      End
    Case ""
      Break
    Default
      FinError("Wrong sintaxis, try --help")
  EndSelect
ForEver

If out_archivo$ = ""
  ;si no se indica archivo de salida, se copia el de entrada quitándole la extensión
  out_archivo$ = in_archivo$
  extension$=GetExtensionPart(out_archivo$)
  If Len(extension$) > 0
    out_archivo$ = Left(out_archivo$, Len(out_archivo$) - Len(extension$) - 1)
  EndIf
Else
  ;si se indicó y tiene extensión tap, se le quita, para tener después el nombre
  extension$ = LCase(GetExtensionPart(out_archivo$))
  If extension$ = "tap"
    out_archivo$ = Left(out_archivo$, Len(out_archivo$) - Len(extension$) - 1)
  EndIf
EndIf

    
;Mapa del nombre de los condactos con su nombre asignado, para una búsqueda rápida
Restore condactos_nombres
For g=0 To 107
  Read.s nombre_condacto.s
  condactos_defs(Mid(nombre_condacto,2))\numero = g
  condactos_defs()\num_argumentos = Val(Left(nombre_condacto,1))
Next

;creamos la estructura
aventura.SComunesToDump

If extract
  ExtractFromFile(in_archivo$, out_archivo$, condactos_defs(), typeextract)
  End
EndIf

If control = #True
  inpaws = RunProgram("inpaws.exe","cp "+ in_archivo$ + " -o " + out_archivo$ + ".ppr" + symbols$, "", #PB_Program_Wait | #PB_Program_Open)
  If inpaws
    If ProgramExitCode(inpaws) = 0
      in_archivo$ = out_archivo$ + ".ppr"
    Else
      FinError("Error from Inpaws. Try usin Inpaws cp "+in_archivo$ + " -o "+ out_archivo$ + ".ppr  to watch the error")
    EndIf
  Else
    FinError("Something gone grong launching Inpaws to preprocess the file")
  EndIf
EndIf  

;{ CARGA DE PREPROCESADO Y COMPILADO INTERNO
If ReadFile(0,in_archivo$)
  If verbose : PrintN("[*] Input file to process: " + in_archivo$):EndIf
  *inputfile = AllocateMemory(Lof(0)) ;alojamos memoria para todo el archivo
  If ReadData(0,*inputfile,Lof(0))    ;leo todo el contenido del archivo para manejarlo como puntero
    Repeat
    *inputfile = trimspace(*inputfile)
    identificador.String
    *inputfile = getpalabra(*inputfile,@identificador,"")
    Select identificador\s
      Case "DEFAULTS"
        *inputfile = CargaDefaults(*inputfile, @defaults)
        If verbose : PrintN("[+] Defaults compiled"): EndIf
      Case "LOCATIONS"
        *inputfile = CargaText(*inputfile, localidades())
        If verbose : PrintN("[+] Locations: " + StrU(MapSize(localidades())) +" locations"): PrintN("    - Descriptions loaded"):EndIf
      Case "CONNECTIONS"
        *inputfile = CargaConexiones(*inputfile, conexiones())
        If verbose : Print("    - Connections loaded: ") : suma = 0 : ForEach conexiones(): suma + ListSize(conexiones()\enlace()): Next: PrintN(StrU(suma)+" conections"):EndIf
      Case "VOCABULARY"
        *inputfile = CargaVocabulario(*inputfile, vocabulario())
        If verbose : PrintN("[+] Vocabulary loaded: " + StrU(ListSize(vocabulario()))) : EndIf
      Case "OBJECTS"
        *inputfile = CargaText(*inputfile, objetos())
        If verbose : PrintN("[+] Objects loaded: " + StrU(MapSize(objetos()))): PrintN("    - Descriptions loaded") : EndIf
      Case "INITIALLYAT"
        *inputfile = CargaObjetosInicial(*inputfile, objetos_inicial())
        If verbose : PrintN("    - Initiallyat loaded"):EndIf
      Case "OBJWORDS"
        *inputfile = CargaObjetosPalabras(*inputfile, objetos_inicial())
        If verbose : PrintN("    - Objects words loaded"):EndIf
      Case "OBJWEIGHT"
        *inputfile = CargaObjetosPeso(*inputfile, objetos_inicial())
        If verbose : PrintN("    - Objects weight loaded"):EndIf
      Case "MESSAGES"
        *inputfile = CargaText(*inputfile, mensajes())
        If verbose : PrintN("[+] Messages loaded: "+StrU(MapSize(mensajes()))+" messages"):EndIf
      Case "SYSMESSAGES"
        *inputfile = CargaText(*inputfile, sysmensajes())
        If verbose : PrintN("[+] System messages loaded: "+StrU(MapSize(sysmensajes()))+" messages"):EndIf
      Case "GRAPHICS"
        *inputfile = CargaGraficos(*inputfile, graficos())
        If verbose : PrintN("[+] Graphics loaded: "+StrU(ListSize(graficos()))+" graphics"):EndIf
        If ListSize(graficos()) <> MapSize(localidades())
          rellenagraficos(graficos(), MapSize(localidades()), 7) ;si hay mas localidades que datos graficos, se rellena con los necesarios
        EndIf
      Case "CHARACTERS"
        *inputfile = CargaCaracteres(*inputfile, udg(), fonts())
        If verbose : PrintN("[+] Characters loaded:") : PrintN("    - UDG loaded"): PrintN("    - Fonts loaded: "+StrU(ListSize(fonts())/768)): EndIf
      Case "PROCESS_TABLE"
        *inputfile = CargaProcesos(*inputfile, procesos())
        If verbose : PrintN("[+] Processes loaded: " + StrU(ListSize(procesos())) + " processes"): EndIf
      Case "ABBREVIATIONS"
        *inputfile = CargaText(*inputfile, abreviaturas())
        ForEach abreviaturas()
          ;esto es para quitar el caracter x1F de fin de cadena que se pone al meter textos
          abreviaturas()\tamano - 1
          ;PokeA(abreviaturas()\mensaje + abreviaturas()\tamano, 0) ;Aunque se puede poner este poke, no es necesario estrictamente
        Next
        If verbose : PrintN("[+] Abbreviations loaded") : EndIf
        ;esta es la ultima seccion y provocamos la salida del repeat
        Break
      Default
        FinError("Wrong sintaxis, part not recognized: " + identificador\s)
    EndSelect
  ForEver
  Else
    FinError("Couldn't read the file")
  EndIf
Else
  FinError("File not found")
EndIf
;}

If Not verbose
  PrintN("[*] Structure loaded")
  PrintN("    Locations:" + StrU(MapSize(localidades())))
  PrintN("    Messages:" + StrU(MapSize(mensajes())))
  PrintN("    Objects:" + StrU(MapSize(objetos())))
  PrintN("    Words on vocabulary:" + StrU(ListSize(vocabulario())))
  PrintN("    System messages:" + StrU(MapSize(sysmensajes())))
  PrintN("    Processes:" + StrU(ListSize(procesos())))
  PrintN("    Fonts:" + StrU(ListSize(fonts())/768))
EndIf


;tras cargar todas las partes de la base de datos, toca volcar a las paginas de memoria
pagina = 0  ;las partes principales van en la pagina 0; y cuando esta se llene habrá que ir habilitando las siguientes
For g = 1 To 7
  paginas_mensajes(g) = 255 ;con esto marco la pagina como deshabilitada
  paginas_localidades(g) = 255  ;con esto marco la pagina como deshabilitada
Next g

;creo una copia para accesos rapidos a toda la estructura desde funciones
CopyMap(objetos(),aventura\objetos())
CopyMap(objetos_inicial(), aventura\objetos_inicial())
CopyList(vocabulario(),aventura\vocabulario())
CopyList(procesos(), aventura\procesos())
CopyMap(sysmensajes(), aventura\sysmensajes())
CopyMap(mensajes(), aventura\mensajes())
CopyMap(localidades(), aventura\localidades())
CopyMap(conexiones(), aventura\conexiones())
CopyList(graficos(), aventura\graficos())


page_base = $9300 ;es la base para la pagina 0

puntero_datos = page_base

;{ VOLCADO DE LOS UDG
;Base de datos: 0 .. 279 : UDGs
ForEach udg()
  poke(pagina, puntero_datos, udg())
  puntero_datos + 1
Next
;}

;280 : valor x14 hardcoded - no se usa en la base de datos, pero la BD inicial la marca con este valor
poke(pagina, puntero_datos, 20)
puntero_datos + 1
;281 : charset default
poke(pagina, puntero_datos, defaults\charset)
;282 - 295 : marcadores de paginación para localidades
;296 - 309 : marcadores de paginacion para mensajes
;310 - valor x00 hardcoded
puntero_datos = page_base + 310
poke(0,puntero_datos,0)
puntero_datos + 1
;{ VOLCADO DE DEFAULTS COLOURS
poke(0,puntero_datos,16)  ;ink ascii control code
poke(0,puntero_datos+1,defaults\ink)
poke(0,puntero_datos+2,17)  ;paper ascii control code
poke(0,puntero_datos+3,defaults\paper)
poke(0,puntero_datos+4,18)  ;flash ascii control code
poke(0,puntero_datos+5,defaults\flash)
poke(0,puntero_datos+6,19)  ;bright ascii control code
poke(0,puntero_datos+7,defaults\bright)
poke(0,puntero_datos+8,20)  ;inverse ascii control code
poke(0,puntero_datos+9,defaults\inverse)
poke(0,puntero_datos+10,21)  ;over ascii control code
poke(0,puntero_datos+11,defaults\over)
;BORDER
poke(0,puntero_datos+12,defaults\border)
;}
;{ VOLCADO DE NUMERO DE ELEMENTOS GENERALES (globales a toda la BD)
;ahora viene el momento de poner los indicadores de numero de elementos generales (de toda la BD)
poke(0, page_base + 324, MapSize(objetos()) )
poke(0, page_base + 325, MapSize(localidades()) )
poke(0, page_base + 326, MapSize(mensajes()))
poke(0, page_base + 327, MapSize(sysmensajes()))
poke(0, page_base + 328, ListSize(procesos()))
poke(0, page_base + 329, ListSize(fonts()) / 768)

;}

;hay dos punteros justo a continuacion +330 y +332, que tienen el offset de los fonts, y de las abreviaturas
;y después a partir de +334 se pueden volcar los datos ordenadamente.
;en la pagina 0 primero volcamos los generales: fonts y abreviaturas
;
;{ VOLCADO DE FONTS
puntero_datos = page_base + 334
poke_w(0,page_base + 330,puntero_datos)
If verbose = 2: PrintN("[-] Offset of fonts:" + Hex(puntero_datos,#PB_Word)): EndIf
ForEach fonts()
  poke(0,puntero_datos,fonts())
  puntero_datos + 1
Next
;}
;{ VOLCADO DE ABREVIATURAS
poke_w(0,page_base + 332,puntero_datos)
If verbose = 2 : PrintN("[-] Offset of abbreviatures:" + Hex(puntero_datos,#PB_Word)): EndIf
If MapSize(abreviaturas()) > 0
  For g = 164 To 254
    FindMapElement(abreviaturas(),StrU(g))
    For i = 0 To abreviaturas()\tamano - 1
      If i <> abreviaturas()\tamano - 1
        poke(0,puntero_datos,PeekA(abreviaturas()\mensaje + i))
      Else
        poke(0,puntero_datos,PeekA(abreviaturas()\mensaje + i) | $80)
      EndIf
      puntero_datos + 1
    Next i
  Next g
Else
  poke(0,puntero_datos,255)
  puntero_datos + 1
EndIf
;}
;{ VOLCADO DE PROCESOS (van todos en la pagina 0)
puntero_datos = DumpProcesos(pagina, puntero_datos, procesos())
;}

numlocalidades = MapSize(localidades())
nummensajes = MapSize(mensajes())
fin_calculado = GetFreePointer(puntero_datos, 0, numlocalidades - 1, 0, nummensajes - 1, @aventura)

;HASTA ESTE PPUNTO EL VOLCADO EL COMUN PARA 48Kb y 128kb
;ahora hay que CALCULAR TAMAÑO REQUERIDO EN LA PAGINA 0
;
;Si no se pisan -> generar BD de 48kb
;Si se pisan -> reparartir para paginar, y crear una BD de 128kb

;

;
tamanno_graficos = GetTamanoGraficos(graficos(), 0, numlocalidades - 1)
inicio_graficos = GetStartGrpahics(graficos(), 0, numlocalidades - 1)
;
;
;DUMP 128kb
;preparamos los elementos dummy, que son estructuras vacías, y que sirven para inicializar las partes
;de la BD que en las páginas accesorias no se usan
*emptymes = AllocateMemory(1)
PokeA(*emptymes,$1F)  ;el terminador de mensaje
NewList procesos_dummy.SProceso()
AddElement(procesos_dummy())
procesos_dummy()\numero = 0
NewMap texto_dummy.STexto() ;creo uno común para cuando sea necesario
texto_dummy("0")\tamano = 1
texto_dummy()\mensaje = *emptymes
NewMap conexiones_dummy.SConexion() ;se crea la localidad 0 sin conexiones
conexiones_dummy("0")
NewList vocabulario_dummy.SVocablo()  ;se crea el vocabulario vacío
NewMap objetos_dummy.SObjeto()        ;se crea un objeto con esta definicion
objetos_dummy("0")\nombre = 255
objetos_dummy()\adjetivo = 0
objetos_dummy()\peso = 1
objetos_dummy()\inicialmente = 252
NewList graficos_dummy.SGrafico()
AddElement(graficos_dummy())
graficos_dummy()\atributo = 1
graficos_dummy()\tamano = 1
AddElement(graficos_dummy()\bytecoded())
graficos_dummy()\bytecoded() = 7

locini = 0
mesini = 0

If mode_smart
  ;modo SMART
  ;primero intentamos meter todos los mensajes posibles en la pagina
  If verbose : PrintN("[*] Smart mode..."): EndIf
  While (mesini < nummensajes Or locini < numlocalidades) And pagina < 8
    mesfin = mesini
    locfin = locini
    If pagina <> 0
      page_base = $c000
      puntero_datos = $c000 + 7
    EndIf ;en caso de que no sea la pagina 0, este es el puntero tras el proceso vacío
    If mesini < nummensajes
      paginas_mensajes(pagina) = mesini
      Repeat
        If locini < numlocalidades
          fin_calculado = GetFreePointer(puntero_datos, locini,locfin,mesini,mesfin, @aventura)
          inicio_graficos = GetStartGrpahics(graficos(), locini, locfin)
        Else
          CopyMap(texto_dummy(),aventura\localidades())
          CopyMap(conexiones_dummy(), aventura\conexiones())
          fin_calculado = GetFreePointer(puntero_datos, 0,0, mesini, mesfin, @aventura)
          inicio_graficos = GetStartGrpahics(graficos_dummy(),0,0)
        EndIf
        If fin_calculado >= inicio_graficos : Break: EndIf
        mesfin + 1
      Until mesfin >= nummensajes
      mesfin - 1
    Else
      paginas_mensajes(pagina) = 255
    EndIf
    If locini < numlocalidades
      paginas_localidades(pagina) = locini
      Repeat
        If mesini < nummensajes
          fin_calculado = GetFreePointer(puntero_datos, locini, locfin, mesini, mesfin, @aventura)  
        Else
          CopyMap(texto_dummy(), aventura\mensajes())
          fin_calculado = GetFreePointer(puntero_datos, locini, locfin, 0, 0, @aventura)
        EndIf
        inicio_graficos = GetStartGrpahics(graficos(), locini, locfin)
        If fin_calculado >= inicio_graficos : Break: EndIf
        locfin + 1
      Until locfin >= numlocalidades
      locfin - 1
      
    Else
      paginas_localidades(pagina) = 255
    EndIf
    ;{ VOLCADO DE OBJETOS (solo los textos de los mismos)
    ;los textos se vuelcan mediante un proceso propio
    ;que me permite volcar un rango dado (para después volcar los necesarios en la paginación)
    ;
    If verbose = 2 : PrintN("[+] Offset for objects text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, aventura\objetos(), 0, MapSize(aventura\objetos()) - 1, $ffdb, $fffa)
    If verbose = 2 : PrintN("[-] Index for objects text: " + Hex(memoria(pagina, $ffdb) + (256*memoria(pagina, $ffdc)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE LOCALIDADES
    If verbose = 2 : PrintN("[+] Offset for locations text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, aventura\localidades(), locini, locfin, $ffdd, $fffb)
    If verbose = 2 : PrintN("[-] Index for locations text: " + Hex(memoria(pagina, $ffdd) + (256*memoria(pagina, $ffde)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE MENSAJES
    If verbose = 2: PrintN("[+] Offset for messages text:" + Hex(puntero_datos,#PB_Word)):EndIf
    If mesini < nummensajes
      puntero_datos = DumpTexto(pagina, puntero_datos, aventura\mensajes(), mesini, mesfin, $ffdf, $fff8)
    Else
      puntero_datos = DumpTexto(pagina, puntero_datos, texto_dummy(), 0,0,$ffdf, $fff8)
    EndIf
    
    If verbose = 2 : PrintN("[-] Index for messages text: " + Hex(memoria(pagina, $ffdf) + (256*memoria(pagina, $ffe0)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE SYSTEM MENSAJES
    If verbose = 2: PrintN("[+] Offset for sysmessages text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, aventura\sysmensajes(), 0, MapSize(aventura\sysmensajes()) - 1, $ffe1, $fff9)
    If verbose = 2 : PrintN("[-] Index for messages text: " + Hex(memoria(pagina, $ffe1) + (256*memoria(pagina, $ffe2)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE CONEXIONES
    If verbose = 2 : PrintN("[+] Offset for conections:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpConexiones(pagina, puntero_datos, aventura\conexiones(), locini, locfin, $ffe3)
    If verbose = 2 : PrintN("[-] Index for conections: " + Hex(memoria(pagina, $ffe3) + (256*memoria(pagina, $ffe4)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE VOCABULARIO
    If verbose = 2 : PrintN("[+] Offset for vocabulary:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpVocabulary(pagina, puntero_datos, aventura\vocabulario(), $ffe5)
    If verbose = 2 : PrintN("[-] Index for vocabulary: " + Hex(memoria(pagina, $ffe5) + (256*memoria(pagina, $ffe6)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE INICIALMENTE
    If verbose = 2 : PrintN("[+] Offset for initiallyat:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpInitiallyat(pagina, puntero_datos, aventura\objetos_inicial(), 0, MapSize(aventura\objetos()) - 1, $ffe7)
    If verbose = 2 : PrintN("[-] Index for initiallyat: " + Hex(memoria(pagina, $ffe7) + (256*memoria(pagina, $ffe8)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE OBJ_PALABRAS
    If verbose = 2 : PrintN("[+] Offset for object words:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpObjPalabras(pagina, puntero_datos, aventura\objetos_inicial(), 0, MapSize(aventura\objetos()) - 1, $ffe9)
    If verbose = 2 : PrintN("[-] Index for object words: " + Hex(memoria(pagina, $ffe9) + (256*memoria(pagina, $ffea)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE OBJ PESOS
    If verbose = 2 : PrintN("[+] Offset for object weights:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpObjPesos(pagina, puntero_datos, aventura\objetos_inicial(), 0, MapSize(aventura\objetos()) - 1, $ffeb)
    If verbose = 2 : PrintN("[-] Index for object weight: " + Hex(memoria(pagina, $ffeb) + (256*memoria(pagina, $ffec)),#PB_Word)): EndIf
    ;}
    poke_w(pagina,$ffed, puntero_datos)
    inicio_graficos = GetStartGrpahics(graficos(), locini, locfin)
    If verbose : Print("[*] RAM " + StrU(pagina)+ " - "): PrintN(StrU(inicio_graficos - puntero_datos - 1) + " bytes free") : EndIf
    If verbose = 2: PrintN("[-] Start of free space: " + Hex(puntero_datos, #PB_Word)):EndIf
    ;{ VOLCADO DE GRAFICOS
    If verbose = 2 : PrintN("[-] Offset for Graphics data:" + Hex(inicio_graficos,#PB_Word)):EndIf
    finvolcado = DumpGraficos(pagina, inicio_graficos, aventura\graficos(),locini, locfin,$fff1, $fff3)
    ;}
    poke(pagina, $fff7, 1)
    poke_w(pagina, $fffd, page_base)  ;esto hay que hacerlo en todas las paginas
    poke(pagina,$ffff,pagina)         ;esto indica que la pagina está inicializada  
    pagina = nextpagina(pagina)
    If pagina <> 0
      CopyList(procesos_dummy(),aventura\procesos())
      CopyMap(texto_dummy(), aventura\objetos())
      CopyMap(texto_dummy(),aventura\sysmensajes())
      CopyList(vocabulario_dummy(), aventura\vocabulario())
      CopyMap(objetos_dummy(), aventura\objetos_inicial())
    EndIf
    mesini = mesfin + 1
    locini = locfin + 1
  Wend
Else
  ;modo AMPLIO: en la pagina 0 solo va localidad 0 y mensaje 0
  If verbose : PrintN("[*] Structured mode..."): EndIf
  mesfin = mesini
  locfin = locini
  fin_calculado = GetFreePointer(puntero_datos, locini, locfin, mesini, mesfin, @aventura)
  inicio_graficos = GetStartGrpahics(graficos(), locini, locfin)
  If inicio_graficos >= fin_calculado
    ;cabe y se vuelca
    poke_w(pagina, $ffd5, 0)  ;valores hardcodeados
    poke_w(pagina, $ffd7, 0)
    poke(pagina, $fff7, 1)
    ;{ VOLCADO DE OBJETOS (solo los textos de los mismos)
    ;los textos se vuelcan mediante un proceso propio
    ;que me permite volcar un rango dado (para después volcar los necesarios en la paginación)
    ;
    If verbose = 2 : PrintN("[+] Offset for objects text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, objetos(), 0, MapSize(objetos()) - 1, $ffdb, $fffa)
    If verbose = 2: PrintN("[-] Index for objects text: " + Hex(memoria(pagina, $ffdb) + (256*memoria(pagina, $ffdc)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE LOCALIDADES
    If verbose = 2 : PrintN("[+] Offset for locations text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, localidades(), locini, locfin, $ffdd, $fffb)
    If verbose = 2 : PrintN("[-] Index for locations text: " + Hex(memoria(pagina, $ffdd) + (256*memoria(pagina, $ffde)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE MENSAJES
    If verbose = 2 : PrintN("[+] Offset for messages text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, mensajes(), mesini, mesfin, $ffdf, $fff8)
    If verbose = 2 : PrintN("[-] Index for messages text: " + Hex(memoria(pagina, $ffdf) + (256*memoria(pagina, $ffe0)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE SYSTEM MENSAJES
    If verbose = 2 : PrintN("[+] Offset for sysmessages text:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpTexto(pagina, puntero_datos, sysmensajes(), 0, MapSize(sysmensajes()) - 1, $ffe1, $fff9)
    If verbose = 2 : PrintN("[-] Index for messages text: " + Hex(memoria(pagina, $ffe1) + (256*memoria(pagina, $ffe2)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE CONEXIONES
    If verbose = 2 : PrintN("[+] Offset for conections:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpConexiones(pagina, puntero_datos, conexiones(), locini, locfin, $ffe3)
    If verbose = 2 : PrintN("[-] Index for conections: " + Hex(memoria(pagina, $ffe3) + (256*memoria(pagina, $ffe4)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE VOCABULARIO
    If verbose = 2 : PrintN("[+] Offset for vocabulary:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpVocabulary(pagina, puntero_datos, vocabulario(), $ffe5)
    If verbose = 2 : PrintN("[-] Index for vocabulary: " + Hex(memoria(pagina, $ffe5) + (256*memoria(pagina, $ffe6)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE INICIALMENTE
    If verbose = 2 : PrintN("[+] Offset for initiallyat:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpInitiallyat(pagina, puntero_datos, objetos_inicial(), 0, MapSize(objetos()) - 1, $ffe7)
    If verbose = 2 : PrintN("[-] Index for initiallyat: " + Hex(memoria(pagina, $ffe7) + (256*memoria(pagina, $ffe8)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE OBJ_PALABRAS
    If verbose = 2 : PrintN("[+] Offset for object words:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpObjPalabras(pagina, puntero_datos, objetos_inicial(), 0, MapSize(objetos()) - 1, $ffe9)
    If verbose = 2 : PrintN("[-] Index for object words: " + Hex(memoria(pagina, $ffe9) + (256*memoria(pagina, $ffea)),#PB_Word)): EndIf
    ;}
    ;{ VOLCADO DE OBJ PESOS
    If verbose = 2 : PrintN("[+] Offset for object weights:" + Hex(puntero_datos,#PB_Word)):EndIf
    puntero_datos = DumpObjPesos(pagina, puntero_datos, objetos_inicial(), 0, MapSize(objetos()) - 1, $ffeb)
    If verbose = 2 : PrintN("[-] Index for object weight: " + Hex(memoria(pagina, $ffeb) + (256*memoria(pagina, $ffec)),#PB_Word)): EndIf
    ;}
    ;en este momento se termina el volcado de la parte baja de la BD
    ;se marcha el puntero de inicio de memoria libre
    poke_w(pagina,$ffed, puntero_datos)
    If verbose : Print("[*] RAM " + StrU(pagina)+ " - "): PrintN(StrU(inicio_graficos - puntero_datos - 1) + " bytes free") : EndIf
    If verbose = 2 : PrintN("[-] Start of free space: " + Hex(puntero_datos, #PB_Word)):EndIf
    ;{ VOLCADO DE GRAFICOS
    If verbose = 2 : PrintN("[-] Offset for Graphics data:" + Hex(inicio_graficos,#PB_Word)):EndIf
    finvolcado = DumpGraficos(pagina, inicio_graficos, graficos(),locini, locfin,$fff1, $fff3)
    ;}
    poke(pagina, $fff7, 1)
    poke_w(pagina, $fffd, page_base)  ;esto hay que hacerlo en todas las paginas
    poke(pagina,$ffff,pagina)         ;esto indica que la pagina está inicializada
    
    ;a partir de la pagina 1, hay que usar dummys para la BD basica que inicializa las paginas
    CopyList(procesos_dummy(), aventura\procesos())
    CopyMap(texto_dummy(), aventura\objetos())
    CopyMap(texto_dummy(), aventura\sysmensajes())
    CopyList(vocabulario_dummy(), aventura\vocabulario())
    CopyMap(objetos_dummy(), aventura\objetos_inicial())
    While  mesini < nummensajes - 1
      pagina = nextpagina(pagina)
      If pagina > 7: FinError("[!] Too much info to place on 128kb, try smart mode"):EndIf
      locini + 1
      locfin = locini
      mesini + 1
      mesfin = mesini
      paginas_mensajes(pagina) = mesini
      paginas_localidades(pagina) = locini
      Repeat
        puntero_base = $c000
        tamano_procesos = 7 ;el tamaño de un proceso vacío
        fin_calculado = tamano_procesos + GetFreePointer(puntero_base, locini, locfin, mesini, mesfin, @aventura)
        inicio_graficos = GetStartGrpahics(graficos(), locini, locfin)
        If fin_calculado >= inicio_graficos : Break: EndIf
        mesfin + 1
      Until mesfin = nummensajes
      mesfin - 1
      ;solo los volcamos si son muy grandes y ellos solos ocupan ya una pagina entera, si no rellenamos esta pagina con localidades
      If mesfin < nummensajes - 1
        puntero = DumpProcesos(pagina,puntero_base, procesos_dummy())
        puntero = DumpTexto(pagina, puntero, texto_dummy(),0,0,$ffdb, $fffa)  ;texto de objetos
        If locini < numlocalidades
          puntero = DumpTexto(pagina, puntero, localidades(), locini, locfin, $ffdd, $fffb) ;texto localidades
        Else
          puntero = DumpTexto(pagina, puntero, texto_dummy(), 0,0,$ffdd, $fffb)
          memoria(0,37957) + 1
        EndIf
        puntero = DumpTexto(pagina,puntero, mensajes(), mesini, mesfin,$ffdf, $fff8)  ;texto mensajes
        puntero = DumpTexto(pagina, puntero, texto_dummy(),0,0,$ffe1, $fff9)          ;mensajes de sistema
        If locini < numlocalidades
          puntero = DumpConexiones(pagina, puntero, conexiones(),locini, locfin, $ffe3) ;conexiones
        Else
          puntero = DumpConexiones(pagina, puntero, conexiones_dummy(), 0,0,$ffe3)
        EndIf
        puntero = DumpVocabulary(pagina, puntero, vocabulario_dummy(),$ffe5)
        puntero = DumpInitiallyat(pagina, puntero, objetos_dummy(), 0,0, $ffe7)
        puntero = DumpObjPalabras(pagina, puntero, objetos_dummy(), 0,0, $ffe9)
        puntero = DumpObjPesos(pagina, puntero, objetos_dummy(), 0,0, $ffeb)
        poke_w(pagina,$ffed, puntero) ;free space
        If locini < numlocalidades
          inicio_graficos = GetStartGrpahics(graficos(),locini, locfin)
          DumpGraficos(pagina,inicio_graficos, graficos(),locini,locfin,$fff1, $fff3)
        Else
          inicio_graficos = GetStartGrpahics(graficos_dummy(),0, 0)
          DumpGraficos(pagina,inicio_graficos, graficos_dummy(),0,0,$fff1, $fff3)
        EndIf
        If verbose : Print("[*] RAM " + StrU(pagina)+ " - "): PrintN(StrU(inicio_graficos - puntero - 1) + " bytes free") : EndIf
        poke(pagina, $fff7, 1)            ;valor hardcodeado
        poke_w(pagina, $fffd, puntero_base)  ;esto hay que hacerlo en todas las paginas
        poke(pagina,$ffff,pagina)            ;esto indica que la pagina está inicializada
        mesini = mesfin
      Else
        ;si estamos ahora en el ultimo bloque de mensajes salimos para rellenar con localidades
        Break
      EndIf
      
    Wend
    ;ahora hay que hacer practicamente lo mismo hasta que se vuelquen todas las localidades
    ;
    While locini < numlocalidades - 1 Or mesini < nummensajes - 1
      locfin = locini
      paginas_localidades(pagina) = locini
      Repeat
        puntero_base = $c000
        tamano_procesos = 7 ;el tamaño de un proceso vacío
        If mesini < nummensajes
          fin_calculado = tamano_procesos + GetFreePointer(puntero_base, locini, locfin, mesini,mesfin,@aventura)
        Else
          CopyMap(texto_dummy(),aventura\mensajes())
          fin_calculado = tamano_procesos + GetFreePointer(puntero_base, locini, locfin, 0, 0, @aventura)
        EndIf
        inicio_graficos = GetStartGrpahics(graficos(),locini, locfin)
        If fin_calculado >= inicio_graficos : Break:EndIf
        locfin + 1
      Until locfin = numlocalidades
      locfin - 1
      puntero = DumpProcesos(pagina,puntero_base, procesos_dummy())
      puntero = DumpTexto(pagina, puntero, texto_dummy(),0,0,$ffdb, $fffa)  ;texto de objetos      
      puntero = DumpTexto(pagina, puntero, localidades(), locini, locfin, $ffdd, $fffb) ;texto localidades
      If mesini < nummensajes
        puntero = DumpTexto(pagina,puntero, mensajes(), mesini, mesfin,$ffdf, $fff8)  ;texto mensajes
      Else
        puntero = DumpTexto(pagina, puntero,texto_dummy(),0,0,$ffdf, $fff8)
      EndIf
      puntero = DumpTexto(pagina, puntero, texto_dummy(),0,0,$ffe1, $fff9)          ;mensajes de sistema
      puntero = DumpConexiones(pagina, puntero, conexiones(), locini,locfin, $ffe3)
      puntero = DumpVocabulary(pagina, puntero, vocabulario_dummy(),$ffe5)
      puntero = DumpInitiallyat(pagina, puntero, objetos_dummy(), 0,0, $ffe7)
      puntero = DumpObjPalabras(pagina, puntero, objetos_dummy(), 0,0, $ffe9)
      puntero = DumpObjPesos(pagina, puntero, objetos_dummy(), 0,0, $ffeb)
      poke_w(pagina,$ffed, puntero) ;free space
      inicio_graficos = GetStartGrpahics(graficos(),locini, locfin)
      DumpGraficos(pagina,inicio_graficos, graficos(),locini,locfin,$fff1, $fff3)
      If verbose : Print("[*] RAM " + StrU(pagina)+ " - "): PrintN(StrU(inicio_graficos - puntero - 1) + " bytes free") : EndIf
      poke(pagina, $fff7, 1)            ;valor hardcodeado
      poke_w(pagina, $fffd, puntero_base)  ;esto hay que hacerlo en todas las paginas
      poke(pagina,$ffff,pagina)            ;esto indica que la pagina está inicializada
      locini  = locfin + 1
      mesini = mesfin + 1
      mesfin = mesini
      pagina = nextpagina(pagina)
      If pagina > 7: FinError("[!] Too much info to place on 128kb, try smart mode"):EndIf
    Wend
  Else
    FinError("[*] No free memory on page 0 to place the esential BD")
  EndIf
  
EndIf

;volcamos las tablas de paginaciones
DumpPaginacion(37914)

;FINALMENTE QUEDA POR VOLCAR LAS PAGINAS EN UN TAP
;la siguiente rutina repasa la configuración de la paginación
;y va seleccionando paginas y volcandolas al TAP segun sea necesario

If control
  DeleteFile(in_archivo$)
EndIf

If CreateFile(0,out_archivo$ + ".tap")
  PrintN("[*] Tap file for output: " + out_archivo$+".tap")
  nombre_archivo$ = GetFilePart(out_archivo$)
  caracter = Asc("A")
  pagina = 0 : numpags=0
  While paginas_localidades(pagina) <> 255 Or paginas_mensajes(pagina) <> 255
    GrabaBloqueEnTap(0,pagina,memoria(pagina,$fffd) + 256*memoria(pagina, $fffe), memoria(pagina, $ffed) + 256*memoria(pagina, $ffee) - 1, LSet(UCase(out_archivo$),9," ")+Chr(caracter))
    GrabaBloqueEnTap(0,pagina,memoria(pagina,$ffef) + 256*memoria(pagina, $fff0), $ffff, LSet(UCase(out_archivo$),9, " ")+Chr(caracter + 1))
    caracter + 2
    numpags + 1
    pagina = nextpagina(pagina)
  Wend
  If numpags = 1
    PrintN("[*] Done! 48k DataBase generated")
  Else
    PrintN("[*] Done! 128k DataBase generated: "+StrU(numpags)+ " pages of RAM used")
  EndIf
Else
  FinError("Couldn't create the output file: " + out_archivo$ + ".tap")
EndIf
End



DataSection
  condactos_nombres:
  Data.s "1AT", "1NOTAT", "1ATGT", "1ATLT", "1PRESENT", "1ABSENT", "1WORN", "1NOTWORN", "1CARRIED", "1NOTCARR", "1CHANCE", "1ZERO", "1NOTZERO", "2EQ", "2GT", "2LT"
  Data.s "1ADJECT1", "1ADVERB", "0INVEN", "0DESC", "0QUIT", "0END", "0DONE", "0OK", "0ANYKEY", "0SAVE", "0LOAD", "0TURNS", "0SCORE", "0CLS"
  Data.s "0DROPALL", "0AUTOG", "0AUTOD", "0AUTOW", "0AUTOR", "1PAUSE", "0TIMEOUT", "1GOTO", "1MESSAGE", "1REMOVE", "1GET", "1DROP", "1WEAR", "1DESTROY", "1CREATE", "2SWAP", "2PLACE"
  Data.s "1SET", "1CLEAR", "2PLUS", "2MINUS", "2LET", "0NEWLINE", "1PRINT", "1SYSMESS", "2ISAT", "2COPYOF", "2COPYOO", "2COPYFO", "2COPYFF", "0LISTOBJ"
  Data.s "1EXTERN", "0RAMSAVE", "1RAMLOAD", "2BEEP", "1PAPER", "1INK", "1BORDER", "1PREP", "1NOUN2", "1ADJECT2"
  Data.s "2ADD", "2SUB", "0PARSE", "1LISTAT", "1PROCESS", "2SAME", "1MES", "1CHARSET", "2NOTEQ", "2NOTSAME", "2MODE", "1LINE", "2TIME", "1PICTURE"
  Data.s "1DOALL", "1PROMPT", "1GRAPHIC", "2ISNOTAT", "2WEIGH", "2PUTIN", "2TAKEOUT", "0NEWTEXT", "2ABILITY", "1WEIGHT", "1RANDOM", "1INPUT", "0SAVEAT", "0BACKAT", "2PRINTAT"
  Data.s "0WHATO", "1RESET", "1PUTO", "0NOTDONE", "1AUTOP", "1AUTOT", "1MOVE", "0PROTECT"
EndDataSection
; IDE Options = PureBasic 5.31 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 1765
; FirstLine = 409
; Folding = EgAAAAAACOAAEA+
; Executable = InpawsPager_x64.exe
; DisableDebugger
; CommandLine = e proces_.tap -o proces_.paw
; Compiler = PureBasic 5.31 (Windows - x64)