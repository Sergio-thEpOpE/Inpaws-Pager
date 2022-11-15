;Paws Graphic Editor


;tipos de comando:
;
Enumeration
  #GR_ABS_MOVE
  #GR_PLOT
  #GR_REL_MOVE
  #GR_LINE
  #GR_BLOCK
  #GR_FILL
  #GR_SHADE
  #GR_GOSUB
  #GR_TEXT
  #GR_INK
  #GR_PAPER
  #GR_BRIGHT
  #GR_FLASH
  #GR_END
EndEnumeration

Structure SStepGrafico
  grafico.l
  step_retorno.l
  escala.l
EndStructure

Structure SComando
  tipo.l
  x.l
  y.l
  ancho.l
  alto.l
  inverse.l
  over.l
  caracter1.l
  caracter2.l
  charset.l
  escala.l
  subrutina.l
  valor.l
  visualizacion.s
EndStructure

Structure SGraph
  tamano.l
  atributo_inicial.a
  List comandos.SComando()
  List bytecodes.a()
EndStructure

Structure SCoordenada
  x.l
  y.l
EndStructure

Structure SColours
  ink.a
  paper.a
  bright.a
  flash.a
EndStructure

XIncludeFile "PawsGraphicEditor.pbf"


Global CharsetSelected, AsciiSelected, ImagenEditor, InvertedFlash, ImagenScreen, in_archivo$
Global GrafEditorBright,GrafEditorFlash, GrafEditorInk, GrafEditorPaper, GrafEditorInverse, GrafEditorOver
Global GraphicSelected
Global LastPoint.SCoordenada, ActualColor.SColours

Global NewList udgs()
Global NewList shades()
Global NewList fonts()
Global NewList font_speccy()
Global NewList graficos.SGraph()
Global NewList pila_subrutinas.SStepGrafico()
Global Dim SpectrumScreen(255,175)
Global Dim SpectrumAttr(31,21)
Global Dim paleta(7,1)  ;primer valor es el color; el segundo valor es el brillo

;avanza el puntero saltando los espacios, tabuladores y saltos de linea
Procedure trimspace(puntero, maximo)
  If puntero >= maximo: ProcedureReturn puntero:EndIf
  caracter = PeekA(puntero)
  While caracter = 32 Or caracter = 9 Or caracter = 10 Or caracter = 13
    puntero + 1
    If puntero >= maximo: Break:EndIf
    caracter = PeekA(puntero)
  Wend
  ProcedureReturn puntero
EndProcedure

;devuelve el puntero avanzado, y una cadena con la palabra hasta que encuentra el separador
Procedure getpalabra(puntero, maximo, *salida.String, separador.s)
  *salida\s = ""
  If puntero >= maximo : ProcedureReturn puntero: EndIf
  caracter = PeekA(puntero)
  While caracter <> Asc(separador.s) And caracter <> 9 And caracter <> 10 And caracter <> 13 And caracter <> 32
    *salida\s = *salida\s + Chr(caracter)
    puntero + 1
    If puntero >= maximo: Break :EndIf
    caracter = PeekA (puntero)
  Wend
  ProcedureReturn puntero
EndProcedure

Procedure isnumber(cadena.s)
  For g=1 To Len(cadena.s)
    If Asc(Mid(cadena.s,g,1)) < 48 Or Asc(Mid(cadena.s,g,1))>57
      ProcedureReturn #False
    EndIf
  Next g
  ProcedureReturn #True
EndProcedure

Procedure CargaCaracteres (puntero,maximo, List udg(), List shades(), List fonts())
  ;intento cargar en un temporal
  NewList temp_udg()
  NewList temp_shades()
  NewList temp_fonts()
  puntero=trimspace(puntero,maximo)
  leida.String
  puntero = getpalabra(puntero,maximo, @leida,"")
  If leida\s <> "{"
    MessageRequester("Error","Missing { on Characters",#PB_MessageRequester_Ok)
    ProcedureReturn 0
  Else
    For g = 0 To 151
      puntero = trimspace(puntero,maximo)
      puntero = getpalabra(puntero,maximo,@leida,",")  ;leemos el numero del localidades presentes
      puntero + 1                                ;avanzamos la coma
      If isnumber(leida\s)
        AddElement(temp_udg())
        temp_udg() = Val(leida\s)
      Else
        MessageRequester("Error","Missing Udgs",#PB_MessageRequester_Ok)
        ProcedureReturn 0
      EndIf
    Next
    For g = 0 To 127
      puntero = trimspace(puntero,maximo)
      puntero = getpalabra(puntero,maximo,@leida,",")  ;leemos el numero del localidades presentes
      puntero + 1                                      ;avanzamos la coma
      If isnumber(leida\s)
        AddElement(temp_shades())
        temp_shades() = Val(leida\s)
      Else
        MessageRequester("Error","Missing Shades",#PB_MessageRequester_Ok)
        ProcedureReturn 0
      EndIf
    Next g
    puntero = trimspace(puntero,maximo)
    puntero = getpalabra(puntero,maximo,@leida,",")  ;leemos el numero del fonts
    puntero +1 ;avanzamos la coma si la hay, y si no será el espacio o retorno de carro
    If isnumber(leida\s)
      numfonts = Val(leida\s)
    Else
      MessageRequester("Error","Missing number on Characters",#PB_MessageRequester_Ok)
      ProcedureReturn 0
    EndIf
    For g = 0 To (numfonts*768) - 1
     puntero = trimspace(puntero,maximo)
      puntero = getpalabra(puntero,maximo, @leida,",")
      puntero + 1 ;avanzamos la coma
      If isnumber(leida\s)
        AddElement(temp_fonts())
        temp_fonts() = Val(leida\s)
      Else
        MessageRequester("Error","Missing number on fonts",#PB_MessageRequester_Ok)
        ProcedureReturn 0
      EndIf
    Next
    puntero = trimspace(puntero,maximo)
    puntero = getpalabra(puntero,maximo, @leida,"")
    If leida\s <> "}"
      MessageRequester("Error","Missing } on Characters",#PB_MessageRequester_Ok)
      ProcedureReturn 0
    EndIf
  EndIf
  ;si no hubo error se copian en los válidos
  CopyList(temp_udg(), udg())
  CopyList(temp_shades(), shades())
  CopyList(temp_fonts(), fonts())
  ClearList(temp_udg())
  ClearList(temp_shades())
  ClearList(temp_fonts())
  ProcedureReturn puntero
EndProcedure

Procedure CargaGraficos (puntero, maximo, List graficos.SGraph())
  ;intento cargar en un temporal
  NewList temp_graficos.SGraph()
  puntero=trimspace(puntero,maximo)
  leida.String
  puntero = getpalabra(puntero,maximo, @leida,"")
  If leida\s <> "{"
    MessageRequester("Error","Missing { on Graphics",#PB_MessageRequester_Ok)
    ProcedureReturn 0
  Else
    puntero = trimspace(puntero, maximo)
    puntero = getpalabra(puntero, maximo, @leida, ",")    
    If leida\s <> "}"
      puntero + 1  ;avanzamos la coma
      numlocalidades = Val(leida\s)
      ;una vez leida el numero de localidades, leemos el tamano de cada grafico
      For g = 0 To numlocalidades - 1
        puntero = trimspace(puntero, maximo)
        puntero = getpalabra(puntero, maximo, @leida,",")
        puntero + 1 ;avanzamos la coma
        If isnumber(leida\s)
          AddElement(temp_graficos())
          temp_graficos()\tamano = Val(leida\s)
        Else
          MessageRequester("Error","Missing number",#PB_MessageRequester_Ok)
          ProcedureReturn 0
        EndIf
      Next
      ;ahora leemos los atributos de cada grafico
      FirstElement(temp_graficos())
      For g = 0 To numlocalidades - 1
        puntero = trimspace(puntero, maximo)
        puntero = getpalabra(puntero, maximo, @leida,",")
        puntero + 1  ;avanzamos la coma
        If isnumber(leida\s)
          temp_graficos()\atributo_inicial = Val(leida\s)
          NextElement(temp_graficos())
        Else
          MessageRequester("Error","Missing number", #PB_MessageRequester_Ok)
          ProcedureReturn 0
        EndIf
      Next
      ;y ahora leemos los datos de cada grafico, para incorporarlos
      FirstElement(temp_graficos())
      For g = 0 To numlocalidades - 1
        For i = 1 To temp_graficos()\tamano
          puntero = trimspace(puntero, maximo)
          puntero = getpalabra(puntero, maximo, @leida,",")
          If isnumber(leida\s)
            AddElement(temp_graficos()\bytecodes())
            temp_graficos()\bytecodes() = Val(leida\s) ;incorporamos el dato del grafico
            puntero + 1                          ;avanza la ,
          Else
            MessageRequester("Error","Missing a number. Graphics data lost on graphic:" + StrU(g), #PB_MessageRequester_Ok)
            ProcedureReturn 0
          EndIf
        Next i
        NextElement(temp_graficos())
      Next g
      puntero = trimspace(puntero, maximo)
      puntero = getpalabra(puntero, maximo, @leida,"")
      If leida\s <> "}"
        MessageRequester("Error","Missing } on graphics", #PB_MessageRequester_Ok)
        ProcedureReturn 0
      EndIf
    EndIf 
  EndIf
  ;si no hubo error se copian en los válidos
  CopyList(temp_graficos(), graficos())
  ClearList(temp_graficos())
  ProcedureReturn puntero
EndProcedure

Procedure ParseaGraficos(List graficos.SGraph())
  ForEach graficos()
    ResetList(graficos()\bytecodes())
    While NextElement(graficos()\bytecodes())
      comando = graficos()\bytecodes()
      AddElement(graficos()\comandos())
      instruccion = comando & %00000111
      Select instruccion
        Case 0  ;abs move / plot
          NextElement(graficos()\bytecodes())
          x = graficos()\bytecodes()
          NextElement(graficos()\bytecodes())
          y = graficos()\bytecodes()
          graficos()\comandos()\inverse = (comando & %00010000) >> 4
          graficos()\comandos()\over = (comando & %00001000) >> 3
          graficos()\comandos()\x = x
          graficos()\comandos()\y = y
          If graficos()\comandos()\inverse = 1 And graficos()\comandos()\over = 1
            graficos()\comandos()\visualizacion = "ABS MOVE "+StrU(graficos()\comandos()\x)+","+StrU(graficos()\comandos()\y)
            graficos()\comandos()\tipo = #GR_ABS_MOVE
          Else
            cadena$="PLOT "
            If graficos()\comandos()\over = 1
              cadena$ + "o"
            EndIf
            If graficos()\comandos()\inverse = 1
              cadena$ + "i"
            EndIf
            graficos()\comandos()\visualizacion = RTrim(cadena$)+" "+StrU(graficos()\comandos()\x)+","+StrU(graficos()\comandos()\y)
            graficos()\comandos()\tipo = #GR_PLOT
          EndIf
        Case 1  ;rel move / line
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\x = graficos()\bytecodes()
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\y = graficos()\bytecodes()
          graficos()\comandos()\inverse = (comando & %00010000) >> 4
          graficos()\comandos()\over = (comando & %00001000) >> 3
          y_signo = (comando & %10000000) >> 7
          x_signo = (comando & %01000000) >> 6
          If x_signo
            graficos()\comandos()\x = -graficos()\comandos()\x
          EndIf
          If y_signo
            graficos()\comandos()\y = -graficos()\comandos()\y
          EndIf
          If graficos()\comandos()\inverse = 1 And graficos()\comandos()\over = 1
            graficos()\comandos()\visualizacion = "REL MOVE "+Str(graficos()\comandos()\x)+","+Str(graficos()\comandos()\y)
            graficos()\comandos()\tipo = #GR_REL_MOVE
          Else
            cadena$ = "LINE "
            If graficos()\comandos()\over = 1
              cadena$ + "o"
            EndIf
            If graficos()\comandos()\inverse = 1
              cadena$ + "i"
            EndIf
            graficos()\comandos()\visualizacion = RTrim(cadena$) + " "+Str(graficos()\comandos()\x)+","+Str(graficos()\comandos()\y)
            graficos()\comandos()\tipo = #GR_LINE
          EndIf
        Case 2  ;FILL, Shade, Bloque
          graficos()\comandos()\inverse = (comando & %00010000) >> 4
          graficos()\comandos()\over = (comando & %00001000) >> 3
          y_signo = (comando & %10000000) >> 7
          x_signo = (comando & %01000000) >> 6
          If comando = $12 ;bloque
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\alto = graficos()\bytecodes()
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\ancho = graficos()\bytecodes()
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\x = graficos()\bytecodes()
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\y = graficos()\bytecodes()
            graficos()\comandos()\visualizacion = "BLOCK "+StrU(graficos()\comandos()\x) + "," + StrU(graficos()\comandos()\y) + " TO "+StrU(graficos()\comandos()\x + graficos()\comandos()\ancho - 1) + ","+StrU(graficos()\comandos()\alto+ graficos()\comandos()\y - 1)
            graficos()\comandos()\tipo = #GR_BLOCK
          Else  ;fill o shade
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\x = graficos()\bytecodes()
            NextElement(graficos()\bytecodes())
            graficos()\comandos()\y = graficos()\bytecodes()
            If x_signo
              graficos()\comandos()\x = -graficos()\comandos()\x
            EndIf
            If y_signo
              graficos()\comandos()\y = -graficos()\comandos()\y
            EndIf
            If (comando & %00100000) >> 5 <> 0 ;shade
              NextElement(graficos()\bytecodes())
              graficos()\comandos()\caracter1 = graficos()\bytecodes() & %00001111
              graficos()\comandos()\caracter2 = (graficos()\bytecodes() & %11110000) >> 4
              coda$= "("+StrU(graficos()\comandos()\caracter1)+","+StrU(graficos()\comandos()\caracter2)+")"
              cadena$="SHADE "
              If graficos()\comandos()\inverse
                cadena$ + "i"
              EndIf
              graficos()\comandos()\visualizacion = RTrim(cadena$)+" "+Str(graficos()\comandos()\x) +","+Str(graficos()\comandos()\y) + " :" + coda$
              graficos()\comandos()\tipo = #GR_SHADE
            Else  ;fill
              graficos()\comandos()\visualizacion = "FILL "+Str(graficos()\comandos()\x) +","+Str(graficos()\comandos()\y)
              graficos()\comandos()\tipo = #GR_FILL
            EndIf
          EndIf
        Case 3  ;gosub
          graficos()\comandos()\escala = (comando & %00111000) >> 3
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\subrutina = graficos()\bytecodes()
          graficos()\comandos()\visualizacion = "GOSUB " + StrU(graficos()\comandos()\subrutina) + " (sc= "+StrU(graficos()\comandos()\escala)+")"
          graficos()\comandos()\tipo = #GR_GOSUB
        Case 4  ;Texto (char, y, x)
          graficos()\comandos()\charset = (comando & %11100000) >> 5
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\caracter1 = graficos()\bytecodes()
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\x = graficos()\bytecodes()
          NextElement(graficos()\bytecodes())
          graficos()\comandos()\y = graficos()\bytecodes()
          graficos()\comandos()\visualizacion = "TEXT AT " + StrU(graficos()\comandos()\y) +", "+ StrU(graficos()\comandos()\x) + "; "
          graficos()\comandos()\tipo = #GR_TEXT
          If graficos()\comandos()\caracter1 > 31 And graficos()\comandos()\caracter1 <144
            graficos()\comandos()\visualizacion = graficos()\comandos()\visualizacion + StrU(graficos()\comandos()\charset) + ":"+Chr(graficos()\comandos()\caracter1)
          Else
            graficos()\comandos()\visualizacion = graficos()\comandos()\visualizacion + StrU(graficos()\comandos()\charset) + ":{" +StrU(graficos()\comandos()\caracter1)+"}"
          EndIf
          
        Case 5  ;Paper / Bright
          graficos()\comandos()\valor = (comando & %01111000) >> 3
          If comando & %10000000 = 0  ;paper
            graficos()\comandos()\visualizacion = "PAPER " + StrU(graficos()\comandos()\valor)
            graficos()\comandos()\tipo = #GR_PAPER
          Else  ;bright
            graficos()\comandos()\visualizacion = "BRIGHT " + StrU(graficos()\comandos()\valor)
            graficos()\comandos()\tipo = #GR_BRIGHT
          EndIf
        Case 6  ;Ink / Flash
          graficos()\comandos()\valor = (comando & %01111000) >> 3
          If comando & %10000000 = 0  ;ink
            graficos()\comandos()\visualizacion = "INK " + StrU(graficos()\comandos()\valor)
            graficos()\comandos()\tipo = #GR_INK
          Else ;flash
            graficos()\comandos()\visualizacion = "FLASH " + StrU(graficos()\comandos()\valor)
            graficos()\comandos()\tipo = #GR_FLASH
          EndIf
        Case 7  ;END
          graficos()\comandos()\visualizacion = "END"
          graficos()\comandos()\tipo = #GR_END
      EndSelect
    Wend
  Next
EndProcedure

Procedure CharsToBmp(imagen, List bytes(), ancho, color_ink, color_paper)
  x = 0
  y = 0
  StartDrawing(ImageOutput(imagen))
  Box(0,0,ImageWidth(imagen),ImageHeight(imagen),RGB(240,240,240))
  ForEach bytes()
    dato = bytes()
    For g = 0 To 7
      If dato & %10000000
        Plot(x + g,y,color_ink)
      Else
        Plot(x + g, y,color_paper)
      EndIf
      dato = dato << 1
    Next g
    y + 1
    If y % 8 = 0
      y - 8
      x + 8
      If x >= ancho
        x = 0
        y = y +8
      EndIf
    EndIf
  Next
  StopDrawing()
EndProcedure

Procedure PonerGrid(imagen, distancia, color)
  StartDrawing(ImageOutput(imagen))
  x = distancia
  ;primero las verticales
  While x < ImageWidth(imagen)
    Line(x,0,1,ImageHeight(imagen), color)
    x + distancia
  Wend
  ;ahora las horizontales
  y = distancia
  While y < ImageHeight(imagen)
    Line(0,y,ImageWidth(imagen),1,color)
    y + distancia
  Wend
  StopDrawing()
EndProcedure

Procedure UpdateGraphicList(graf)
  ClearGadgetItems(ListView_GraphicSteps)
  SelectElement(graficos(), graf)
  ForEach graficos()\comandos()
    AddGadgetItem(ListView_GraphicSteps, -1, graficos()\comandos()\visualizacion)
  Next
EndProcedure

Procedure CloseApp(EventType)
  PostEvent(#PB_Event_CloseWindow)
EndProcedure

Procedure ImageClone(imagen, ancho, alto)
  repetidor = CreateImage(#PB_Any, ancho, alto)
  StartDrawing(ImageOutput(repetidor))
  For x = 0 To Int(ancho / ImageWidth(imagen))
    For y = 0 To Int(alto / ImageHeight(imagen))
      DrawImage(ImageID(imagen), x * ImageWidth(imagen), y * ImageHeight(imagen))
    Next y
  Next x
  StopDrawing()
  ProcedureReturn repetidor
EndProcedure

Procedure UpdateViews(List caracter())
  ResizeImage(ImagenEditor,8,8)
  StartDrawing(ImageOutput(ImagenEditor))
  Box(0,0,8,8,RGB(240,240,240))
  StopDrawing()
  CharsToBmp(ImagenEditor,caracter(),8,RGB(0,0,0),RGB(250,250,250))
  ClearList(caracter())
  ResizeImage(ImagenEditor,16,16,#PB_Image_Raw)
  SetGadgetAttribute(Canvas_Editor_Little,#PB_Canvas_Image,ImageID(ImagenEditor))
  repetidor = ImageClone(ImagenEditor, 128, 128)
  SetGadgetAttribute(Canvas_Editor_Repeat, #PB_Canvas_Image, ImageID(repetidor))
  FreeImage(repetidor)
  ResizeImage(ImagenEditor,256,256,#PB_Image_Raw)
  PonerGrid(ImagenEditor,32, RGB(255,0,0))
  SetGadgetAttribute(Canvas_Editor,#PB_Canvas_Image, ImageID(ImagenEditor))
EndProcedure

Procedure GetBytesFromChar(set, ascii, List caracter())
  Select ascii
    Case 0 To 15  ;solo puede ocurrir con shades
      SelectElement(shades(), ascii * 8)
      For g= 0 To 7
        AddElement(caracter())
        caracter() = shades()
        NextElement(shades())
      Next g
      
    Case 32 To 127  ;charset si existe, si no el de rom
      If set > 0 And ListSize(fonts()) >= set * 768
        SelectElement(fonts(), (set-1) * 768 + (8 * (ascii - 32)))
        For g = 0 To 7
          AddElement(caracter())
          caracter() = fonts()
          NextElement(fonts())
        Next g
      Else
        SelectElement(font_speccy(), (ascii-32) * 8)
        For g = 0 To 7
          AddElement(caracter())
          caracter() = font_speccy()
          NextElement(font_speccy())
        Next g
      EndIf
      
    Case 128 To 143 ;caracteres cuadrados predefinidos
      SelectElement(font_speccy(), (ascii-32) * 8)
      For g = 0 To 7
        AddElement(caracter())
        caracter() = font_speccy()
        NextElement(font_speccy())
      Next g
      
    Case 144 To 162 ;UDG
      SelectElement(udgs(), (ascii - 144) * 8)
      For g = 0 To 7
        AddElement(caracter())
        caracter() = udgs()
        NextElement(udgs())
      Next g
    Default
      For g = 0 To 7
          AddElement(caracter())
          caracter() = 255
        Next g
  EndSelect
EndProcedure

Procedure LoadCharToEditor(set, ascii)
  SetGadgetText(Text_CharsetInfo, "Charset " + StrU(set))
  SetGadgetText(Text_AsciiInfo, "ASCII Code: "+StrU(ascii))
  NewList caracter()
  GetBytesFromChar(set, ascii, caracter())
  UpdateViews(caracter())
  FreeList(caracter())
EndProcedure

Procedure Udgs_Click(EventType)
  Select EventType
    Case #PB_EventType_LeftClick
      x = Int(GetGadgetAttribute(Canvas_Udg,#PB_Canvas_MouseX) / 24)
      y = Int(GetGadgetAttribute(Canvas_Udg,#PB_Canvas_MouseY) / 24)
      CharsetSelected = 0
      AsciiSelected = 144 + y * 10 + x
      LoadCharToEditor(CharsetSelected, AsciiSelected)
  EndSelect
  ProcedureReturn #True
EndProcedure

Procedure Shades_Click(EventType)
  Select EventType
    Case #PB_EventType_LeftClick
      x = Int(GetGadgetAttribute(Canvas_Shades,#PB_Canvas_MouseX) / 24)
      y = Int(GetGadgetAttribute(Canvas_Shades,#PB_Canvas_MouseY) / 24)
      CharsetSelected = 0
      AsciiSelected = y * 8 + x
      LoadCharToEditor(CharsetSelected, AsciiSelected)
  EndSelect
  ProcedureReturn #True
EndProcedure
  
Procedure UpdateUdgs()
  udgs_image = CreateImage(#PB_Any,80,16) ;10 caracteres de ancho, por dos de alto
  CharsToBmp(udgs_image, udgs(), 80, RGB(0,0,0), RGB(250,250,250))
  ResizeImage(udgs_image, 240,48,#PB_Image_Raw)
  PonerGrid(udgs_image, 24, RGB(255,0,0))
  SetGadgetAttribute(Canvas_Udg, #PB_Canvas_Image, ImageID(udgs_image))
  FreeImage(udgs_image)
EndProcedure

Procedure UpdateShades()
  shades_image = CreateImage(#PB_Any,64,16)
  CharsToBmp(shades_image, shades(), 64, RGB(0,0,0), RGB(250,250,250))
  ResizeImage(shades_image, 192, 48, #PB_Image_Raw)
  PonerGrid(shades_image, 24, RGB(255,0,0))
  SetGadgetAttribute(Canvas_Shades, #PB_Canvas_Image, ImageID(shades_image))
  FreeImage(shades_image)
EndProcedure

Procedure UpdateFonts()
  If CharsetSelected <> 0 And ListSize(fonts()) >= CharsetSelected * 768
    imagen_charset = CreateImage(#PB_Any,64,96)
    NewList font_selected()
    For g = 0 To 767
      SelectElement(fonts(),(CharsetSelected - 1) * 768 + g)
      AddElement(font_selected())
      font_selected() = fonts()
    Next g
    CharsToBmp(imagen_charset,font_selected(),64,RGB(0,0,0), RGB(250,250,250))
    ResizeImage(imagen_charset, 192,288,#PB_Image_Raw)
    PonerGrid(imagen_charset, 24, RGB(255,0,0))
    SetGadgetAttribute(Canvas_Font,#PB_Canvas_Image,ImageID(imagen_charset))
    ClearList(font_selected())
    FreeImage(imagen_charset)
    DisableMenuItem(0,#Mn_delete_charset,0)
    DisableMenuItem(0,#Mn_clear_charset,0)
    DisableMenuItem(0,#Mn_exp_font,0)
  Else
    imagen_charset = CreateImage(#PB_Any,64,96)
    StartDrawing(ImageOutput(imagen_charset))
    Box(0,0,64,96,RGB(250,250,250))
    StopDrawing()
    ResizeImage(imagen_charset, 192, 288, #PB_Image_Raw)
    SetGadgetAttribute(Canvas_Font, #PB_Canvas_Image, ImageID(imagen_charset))
    FreeImage(imagen_charset)
    DisableMenuItem(0,#Mn_delete_charset,1)
    DisableMenuItem(0,#Mn_clear_charset,1)
    DisableMenuItem(0,#Mn_exp_font,1)
  EndIf
EndProcedure

Procedure UpdateBytes(x,y,value)
  mask_to_put = value << (7-x)
  mask_to_preserve = (1 << (7 - x)) ! $ff
  Select CharsetSelected
    Case 0
      If AsciiSelected < 16
        SelectElement(shades(),AsciiSelected * 8 + y)
        shades() = (shades() & mask_to_preserve) | mask_to_put
        LoadCharToEditor(CharsetSelected, AsciiSelected)
        UpdateShades()
      ElseIf  AsciiSelected >= 144 And AsciiSelected <= 162
        SelectElement(udgs(), (AsciiSelected - 144) * 8 + y)
        udgs() = (udgs() & mask_to_preserve) | mask_to_put
        LoadCharToEditor(CharsetSelected, AsciiSelected)
        UpdateUdgs()
      EndIf
    Case 1 To 5
      If AsciiSelected >= 32 And AsciiSelected <=127
        SelectElement(fonts(), (CharsetSelected - 1) * 768 + (AsciiSelected - 32)*8 + y)
        fonts() = (fonts() & mask_to_preserve) | mask_to_put
        LoadCharToEditor(CharsetSelected, AsciiSelected)
        UpdateFonts()
      EndIf
  EndSelect
EndProcedure

Procedure Editor_Click(EventType)
  x = Int(GetGadgetAttribute(Canvas_Editor,#PB_Canvas_MouseX) / 32)
  y = Int(GetGadgetAttribute(Canvas_Editor,#PB_Canvas_MouseY) / 32)
  Select EventType
    Case #PB_EventType_LeftButtonDown
      UpdateBytes(x,y,1)
    Case #PB_EventType_RightButtonDown
      UpdateBytes(x,y,0)  
    Case #PB_EventType_MouseMove       
      If GetGadgetAttribute(Canvas_Editor, #PB_Canvas_Buttons) = #PB_Canvas_LeftButton
        UpdateBytes(x,y,1)
      ElseIf GetGadgetAttribute(Canvas_Editor, #PB_Canvas_Buttons) = #PB_Canvas_RightButton
        UpdateBytes(x,y,0)  
      EndIf
  EndSelect
EndProcedure

Procedure Charset_Click(EventType)
  Select EventType
    Case #PB_EventType_LeftClick
      x = Int(GetGadgetAttribute(Canvas_Font,#PB_Canvas_MouseX) / 24)
      y = Int(GetGadgetAttribute(Canvas_Font,#PB_Canvas_MouseY) / 24)
      AsciiSelected = 32 + y * 8 + x
      CharsetSelected = GetGadgetState(Combo_Font) + 1
      LoadCharToEditor(CharsetSelected, AsciiSelected)
  EndSelect
  ProcedureReturn #True
EndProcedure

Procedure Graba(Event)
  
  Shared out_archivo$
  Repeat
    try = 0
    overwrite = 1
    If in_archivo$ = "" Or Event = #Mn_GrabarComo
      out_archivo$ = SaveFileRequester("Save as...","paws.gfx","Paws Gfx|*.gfx|Paws Charset|*.ec|Paws Graphics|*.eg",0)
      try = 1
      If out_archivo$ = "" : ProcedureReturn:EndIf
    Else
      out_archivo$ = in_archivo$
    EndIf
    If LCase(GetExtensionPart(out_archivo$)) = "ec"
      If MessageRequester("Warning","Only Characters, Udgs, and Shades will be saved. Continue?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
        modo$ = "ec"
      Else
        ProcedureReturn
      EndIf
    ElseIf LCase(GetExtensionPart(out_archivo$)) = "eg"
      If MessageRequester("Warning","Only Characters, Udgs, and Shades will be saved. Continue?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
        modo$ = "eg"
      Else
        ProcedureReturn
      EndIf
    Else
      modo$ = "gfx"
    EndIf
    If try
      try = ReadFile(#PB_Any,out_archivo$)
      If try
        CloseFile(try)
        If MessageRequester("Overwrite","Do you want overwrite the existing file?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
          overwrite = 1
        Else
          overwrite = 0
        EndIf
      EndIf
    EndIf
  Until overwrite = 1

  out_file = CreateFile(#PB_Any, out_archivo$)
  If modo$ = "ec" Or modo$="gfx"
    WriteStringN(out_file, "CHARACTERS")
    WriteStringN(out_file, "{")
    coma = #False
    ;primero los udg
    For g = 0 To 279
      If g<152
        SelectElement(udgs(),g)
        dato=udgs()
      Else
        SelectElement(shades(),g-152)
        dato=shades()
      EndIf
      If coma
        WriteString(out_file,", ")
      EndIf
      If g % 8 = 0
        WriteStringN(out_file,"")
        WriteString(out_file, Chr(9))
      EndIf
      WriteString(out_file, StrU(dato))
      coma = #True
    Next g
    
    numerofuentes = Int(ListSize(fonts()) / 768)
    WriteStringN(out_file,",")
    WriteString(out_file, Chr(9) + StrU(numerofuentes))
    For g = 0 To (numerofuentes * 768) -1
      SelectElement(fonts(),g)
      dato = fonts()
      WriteString(out_file,", ")
      If g % 8 = 0
        WriteStringN(out_file,"")
        WriteString(out_file, Chr(9))
      EndIf
      WriteString(out_file, StrU(dato))
    Next g
    WriteStringN(out_file, "")
    WriteStringN(out_file, "}")
    WriteStringN(out_file, "")
  ElseIf modo$= "eg" Or modo$="gfx"
    ;graba graficos
  EndIf
  CloseFile(out_file)
  in_archivo$ = out_archivo$
  SetWindowTitle(Window_0,GetWindowTitle(Window_0) + " - " + in_archivo$)
  EndProcedure
  
Procedure InsertDeleteCharset(Event)
  Select Event
    Case #Mn_Insert_Charset
      numcharsets = Int(ListSize(fonts()) / 768)
      If numcharsets < 5
        LastElement(fonts())
        Restore font_default
        For g = 0 To 767
          AddElement(fonts())
          Read.a fonts()
        Next g
        numcharsets + 1
        AddGadgetItem(Combo_Font,numcharsets - 1,"Charset " + StrU(numcharsets))
        SetGadgetState(Combo_Font, numcharsets - 1)
        PostEvent(#PB_Event_Gadget,Window_0,Combo_Font,#PB_EventType_Change)
      Else
        MessageRequester("Error","Maximum number reached")
      EndIf
    Case #Mn_delete_charset
      If ListSize(fonts()) >= 768
        If MessageRequester("Warning","Are you sure?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
          selected = GetGadgetState(Combo_Font)
          SelectElement(fonts(),selected * 768 + 767)
          For g= 0 To 767
            DeleteElement(fonts())
          Next g
          RemoveGadgetItem(Combo_Font,Int(ListSize(fonts()) / 768))
          selected - 1
          If selected < 0 : selected = 0: EndIf
          SetGadgetState(Combo_Font, selected)
          If CharsetSelected > 0 
            CharsetSelected = selected
            If CharsetSelected = 0
              AsciiSelected = 0
              LoadCharToEditor(0,0)
            EndIf
          EndIf
        PostEvent(#PB_Event_Gadget,Window_0,Combo_Font,#PB_EventType_Change)
        EndIf
      EndIf
    Case #Mn_clear_charset
      If ListSize(fonts()) >= 768
        If MessageRequester("Warning","Are you sure?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
          selected = GetGadgetState(Combo_Font)
          For g= 0 To 767
            SelectElement(fonts(),selected * 768 + g)
            fonts() = 0
          Next g
          PostEvent(#PB_Event_Gadget,Window_0,Combo_Font,#PB_EventType_Change)
        EndIf
      EndIf
  EndSelect
EndProcedure

Procedure ImportFile(Event)
  importfile$ = OpenFileRequester("Select binary to import","","Binary file|*.bin;*.chr;*.udg;*.shd|All files|*.*",0)
  If importfile$ <> ""
    imp_file = ReadFile(#PB_Any,importfile$)
    If imp_file
      tamano = Lof(imp_file)
      Select Event
        Case #Mn_Import_Udg
          If tamano = 152 Or tamano = 168
            ClearList(udgs())
            For g = 0 To 151
              AddElement(udgs())
              udgs() = ReadAsciiCharacter(imp_file)
            Next g
            UpdateUdgs()
          Else
            MessageRequester("Error","This binary file doesn't seem an Udg")
          EndIf
        Case #Mn_Import_Shades
          If tamano = 128
            ClearList(shades())
            For g = 0 To 127
              AddElement(shades())
              shades() = ReadAsciiCharacter(imp_file)
            Next g
            UpdateShades()
          Else
            MessageRequester("Error","This binary file doesn't seem Shades for Paws")
          EndIf
        Case #Mn_Import_Font
          If tamano = 768
            selected = GetGadgetState(Combo_Font)
            If selected >= 0
              For g = 0 To 767
                SelectElement(fonts(),selected * 768 + g)
                fonts() = ReadAsciiCharacter(imp_file)
              Next g
              UpdateFonts()
              LoadCharToEditor(CharsetSelected, AsciiSelected)
            Else
              MessageRequester("Warning","There is no charset created. Insert one, and import on it")
            EndIf
          Else
            MessageRequester("Error","This binary file doesn't seem a Charset")
          EndIf
      EndSelect
      CloseFile(imp_file)
    Else
      MessageRequester("Error","Couldn't open the file",#PB_MessageRequester_Ok)
    EndIf
  EndIf    
EndProcedure

Procedure ExportFile(Event)
  exportfile$ = SaveFileRequester("Select binary to export","","Binary file|*.bin;*.chr;*.udg;*.shd|All files|*.*",0)
  If exportfile$ <> ""
    exp_file = ReadFile(#PB_Any,exportfile$)
    If exp_file
      CloseFile(exp_file)
      exp_file = 0
      If MessageRequester("File exists","Do you want to overwrite?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_No
        ProcedureReturn
      EndIf
    EndIf

    Select Event
      Case #Mn_exp_Udg
        exp_file = CreateFile(#PB_Any,exportfile$)
        ForEach udgs()
          WriteAsciiCharacter(exp_file,udgs())
        Next
      Case #Mn_exp_shades
        exp_file = CreateFile(#PB_Any,exportfile$)
        ForEach shades()
          WriteAsciiCharacter(exp_file,shades())
        Next
      Case #Mn_exp_font
        selected = GetGadgetState(Combo_Font)
        If selected >= 0
          exp_file = CreateFile(#PB_Any,exportfile$)
          For g = 0 To 767
            SelectElement(fonts(), selected * 768 + g)
            WriteAsciiCharacter(exp_file, fonts())
          Next g
        Else
          MessageRequester("Error","There is not any charset created to export")
        EndIf
    EndSelect
    If exp_file
      CloseFile(exp_file)
    EndIf
  EndIf   
EndProcedure

Procedure UpdateFlash()
  image = CopyImage(ImagenScreen, #PB_Any)
  StartDrawing(ImageOutput(image))
  For x = 0 To 31
    For y = 0 To 21
      If SpectrumAttr(x,y) > 127
        attr=SpectrumAttr(x,y)
        ink = attr & %00000111
        paper = (attr & %00111000) >> 3
        bright = (attr & %01000000) >> 6
        flash = (attr & %10000000) >> 7
        If InvertedFlash
          temp = ink
          ink = paper
          paper = temp
        EndIf
        For xx = x*8 To (x*8) + 7
          For yy = y*8 To (y*8) + 7
            If SpectrumScreen(xx,yy)
              Plot (xx,175-yy,paleta(ink,bright))
            Else
              Plot (xx,175-yy,paleta(paper,bright))
            EndIf
          Next yy
        Next xx
      EndIf
    Next y
  Next x
  StopDrawing()
  ResizeImage(image,512, 352, #PB_Image_Raw)
  SetGadgetAttribute(Canvas_Screen, #PB_Canvas_Image, ImageID(image))
  FreeImage(image)
EndProcedure

Procedure ScreenToWindow()
  StartDrawing(ImageOutput(ImagenScreen))
  For y = 0 To 175
    For x = 0 To 255
      If x%8 = 0 Or y%8=0
        attr = SpectrumAttr(Int(x/8),Int(y/8)) 
        ink = attr & %00000111
        paper = (attr & %00111000) >> 3
        bright = (attr & %01000000) >> 6
        flash = (attr & %10000000) >> 7
        If InvertedFlash = 1 And flash
          temp = ink
          ink = paper
          paper = temp
        EndIf
      EndIf
      If SpectrumScreen(x,y) = 1
        Plot(x, 175-y,paleta(ink,bright))
      Else
        Plot(x, 175-y,paleta(paper,bright))
      EndIf
    Next x
  Next y
  StopDrawing()
  image = CopyImage(ImagenScreen, #PB_Any)
  ResizeImage(image,512,352,#PB_Image_Raw)
  SetGadgetAttribute(Canvas_Screen, #PB_Canvas_Image, ImageID(image))
  FreeImage(image)
EndProcedure

Procedure UpdatePaletaSelector()
  SetGadgetState(Button_Bright, GrafEditorBright)
  SetGadgetState(Button_Flash, GrafEditorFlash)
  img_paleta = CreateImage(#PB_Any,9,1)
  StartDrawing(ImageOutput(img_paleta))
  For g=0 To 7
    Plot(g,0,paleta(g, GrafEditorBright))
  Next g
  ;creo un color ficticio para el Color 8 )
  Plot(8,0,RGB(200,145,200))
  StopDrawing()
  ResizeImage(img_paleta,216,24,#PB_Image_Raw)
  StartDrawing(ImageOutput(img_paleta))
  DrawingMode(#PB_2DDrawing_XOr)
  DrawText(GrafEditorInk * 24 + 3, 4, "I")
  DrawText(GrafEditorPaper * 24 + 12, 4, "P")
  StopDrawing()
  SetGadgetAttribute(Canvas_ColorSelector,#PB_Canvas_Image,ImageID(img_paleta))
  FreeImage(img_paleta)
EndProcedure

Procedure Paleta_Click(EventType)
  Select EventType
    Case #PB_EventType_LeftClick
      x = GetGadgetAttribute(Canvas_ColorSelector, #PB_Canvas_MouseX)
      GrafEditorInk = Int(x / 24)
      UpdatePaletaSelector()
    Case #PB_EventType_RightClick
      x = GetGadgetAttribute(Canvas_ColorSelector, #PB_Canvas_MouseX)
      GrafEditorPaper = Int(x / 24)
      UpdatePaletaSelector()
  EndSelect
EndProcedure

Procedure BrightFlashBotones(EventType)
  If EventType = #PB_EventType_LeftClick
    Select EventGadget()
      Case Button_Bright
        GrafEditorBright = GetGadgetState(Button_Bright)
        UpdatePaletaSelector()
      Case Button_Flash
        GrafEditorFlash = GetGadgetState(Button_Flash)
      Case Button_Inverse
        GrafEditorInverse = GetGadgetState(Button_Inverse)
      Case Button_Over
        GrafEditorOver = GetGadgetState(Button_Over)
    EndSelect
  EndIf
EndProcedure

Procedure PutAttrSpeccy(x, y, ink, paper, bright, flash)
  atributo = SpectrumAttr(Int(x/8),Int(y/8))
  If ink < 8
    atributo = (atributo & %11111000) | ink
  EndIf
  If paper < 8
    atributo = (atributo & %11000111) | (paper << 3)
  EndIf
  If bright < 8
    atributo = (atributo & %10111111) | (bright << 6)
  EndIf
  If flash < 8
    atributo = (atributo & %01111111) | (flash << 7)
  EndIf
  SpectrumAttr(Int(x/8),Int(y/8)) = atributo
EndProcedure

Procedure NormalizaPunto(*punto.SCoordenada)
  If *punto\x > 255
    *punto\x - 256
  ElseIf *punto\x < 0
    *punto\x + 256
  EndIf
  If *punto\y > 255
    *punto\y - 256
  ElseIf  *punto\y < 0
    *punto\y + 256
  EndIf
EndProcedure

Procedure PlotSpeccy(valor, x, y, over, inverse, ink, paper, bright, flash, *punto.SCoordenada)
  If valor <> 0 : valor = 1: EndIf
  If over = 0
    SpectrumScreen(x,y) = (valor ! inverse)
  Else
    SpectrumScreen(x,y) = SpectrumScreen(x,y) ! (valor ! inverse)
  EndIf
  PutAttrSpeccy(x,y, ink, paper, bright, flash)
  *punto\x = x
  *punto\y = y
EndProcedure

Procedure LineSpeccy(delta_x, delta_y, over, inverse, ink, paper, bright, flash, *ultimopunto.SCoordenada)
  plotx = *ultimopunto\x
  ploty = *ultimopunto\y
  dx = Sign(delta_x)
  If dx = 0 : dx = 1: EndIf
  dy = Sign(delta_y)
  If dy = 0: dy = 1: EndIf
  x = Abs(delta_x)
  y = Abs(delta_y)
  If x < y
    l = x
    b = y
    ddx = 0
    ddy = dy
  Else
    l = y
    b = x
    ddx = dx: ddy = 0
    If x + y = 0 : ProcedureReturn: EndIf
  EndIf
  h = b
  i = Int(b/2)
  For n = b To 1 Step -1
    i = i + l
    If i < H
      ix = ddx
      iy = ddy
    Else 
      i = i - h
      ix = dx
      iy = dy
    EndIf
    plotx + ix
    ploty + iy
    If x >= 0 And x <= 255 And y >= 0 And y <= 175
      PlotSpeccy(1,plotx,ploty, over, inverse, ink, paper, bright, flash, *ultimopunto)
    Else
      Break
    EndIf
  Next n
EndProcedure

Procedure BoxSpeccy(x_lr, y_lr, ancho, alto, ink, paper, bright, flash)
  For yyy = 0 To alto
    For xxx=0 To ancho
      PutAttrSpeccy(x_lr*8 + xxx*8, (21 - y_lr)*8 - yyy*8, ink, paper, bright, flash)
    Next xxx
  Next yyy
EndProcedure

Procedure PrintCharSpeccy(x_lr, y_lr, charset, ascii, ink, paper, bright, flash, over, inverse)
  dummy.SCoordenada
  y = (21 - y_lr) * 8 + 7
  x = x_lr * 8
  NewList caracter()
  GetBytesFromChar(charset, ascii, caracter())
  FirstElement(caracter())
  For g = 0 To 7
    mascara = %10000000
    For b = 7 To 0 Step -1
      PlotSpeccy((caracter() & mascara) >> b , x + (7 - b), y, over, inverse, ink, paper, bright, flash, @dummy)
      mascara >> 1
    Next b
    NextElement(caracter())
    y = y - 1
  Next g
  FreeList(caracter())
EndProcedure

Procedure Shade(x_delta, y_delta, escala, ascii1, ascii2, ink, paper, bright, flash, inverse, *punto.SCoordenada)
  dummy.SCoordenada
  inicio.SCoordenada
  NewList caracter1()
  NewList caracter2()
  GetBytesFromChar(0, ascii1, caracter1())
  GetBytesFromChar(0, ascii2, caracter2())
  For g = 0 To 7
    SelectElement(caracter1(), g)
    SelectElement(caracter2(), g)
    caracter1() = caracter1() | caracter2()
    If inverse <> 0
      caracter1() = caracter1() ! $ff
    EndIf
  Next g
  NewList pila_relleno.SCoordenada()
  inicio\x = *punto\x + (x_delta * escala / 8)
  inicio\y = *punto\y + (y_delta * escala / 8)
  NormalizaPunto(@inicio)
  x = inicio\x
  y = inicio\y
  
  Dim maximo_x(175)  
  direccion = -1 ;empezamos a rellenar hacia abajo
  
  Repeat
    If y < 0 Or y > 175 : ProcedureReturn : EndIf
    If SpectrumScreen(x,y) <> 0 : ProcedureReturn : EndIf ;si el pixel de inicio ya está relleno, se sale de la rutina
    AddElement(pila_relleno())
    pila_relleno()\x = x
    pila_relleno()\y = y
    Repeat
      topush = 1
      x = pila_relleno()\x
      y = pila_relleno()\y
      DeleteElement(pila_relleno())
      While SpectrumScreen(x,y) = 0 ;primero buscamos el pixel tope por la derecha
        x + 1
        If x > 255 : Break: EndIf
      Wend
      x - 1
      maximo_x(y) = x
      While SpectrumScreen(x,y) = 0
        SelectElement(caracter1(), 7- (y & %00000111))
        relleno = caracter1()
        bit_x = %10000000 >> (x & %00000111)
        bit = relleno & bit_x
        PlotSpeccy(bit, x, y, 0, 0, ink, paper, bright, flash, @dummy)
        ;ScreenToWindow() ;Debug de Shade
        If y + direccion >= 0 And y+direccion <= 175
          If x > maximo_x(y + direccion)
            If SpectrumScreen(x,y+direccion) = 0
              If topush = 1
                If ListSize(pila_relleno()) <=30
                  AddElement(pila_relleno())
                  pila_relleno()\x = x
                  pila_relleno()\y = y + direccion
                  topush = 0
                Else
                  ProcedureReturn
                EndIf
              EndIf
            Else
              topush = 1
            EndIf
          EndIf
        EndIf
        x - 1
        If x < 0 : Break: EndIf
      Wend
    Until LastElement(pila_relleno()) = 0
    x = inicio\x
    y = inicio\y + 1
    direccion + 2
  Until direccion > 1
EndProcedure

Procedure ClsSpeccy(atributo)
  ;este comando borra toda la pantalla
  Dim SpectrumScreen(255,175)
  BoxSpeccy(0,0,31,21, atributo & %0000111, (atributo & %00111000) >> 3, (atributo & %01000000) >> 6, (atributo & %10000000) >> 7)
EndProcedure

Procedure ProcesaGraficos(numero_grafico, step_final, escala, ink, paper, bright, flash)
  Shared ActualColor
  
  If SelectElement(graficos(), numero_grafico)
    If ListSize(pila_subrutinas()) = 0
      ClsSpeccy(graficos()\atributo_inicial & %01111111)
      ScreenToWindow()
      LastPoint\x = 0
      LastPoint\y = 0
      ActualColor\paper = (graficos()\atributo_inicial & %00111000) >> 3
      ActualColor\ink = graficos()\atributo_inicial & %00000111
    Else
      ActualColor\paper= paper
      ActualColor\ink= ink
      ActualColor\bright= bright
      ActualColor\flash= flash
    EndIf
    If escala = 0: escala = 8: EndIf
    If step_final = -1
      step_final = ListSize(graficos()\comandos()) - 1
    EndIf
    For g = 0 To step_final
      If SelectElement(graficos()\comandos(), g)
        Select graficos()\comandos()\tipo
          Case #GR_ABS_MOVE
            With graficos()\comandos()
              PlotSpeccy(1, \x, \y, 1, 1, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, @LastPoint)
            EndWith
          Case #GR_PLOT
            With graficos()\comandos()
              PlotSpeccy(1, \x, \y, \over, \inverse, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, @LastPoint)
            EndWith
          Case #GR_REL_MOVE
            LastPoint\x = LastPoint\x + Int (graficos()\comandos()\x * escala / 8)
            LastPoint\y = LastPoint\y + Int (graficos()\comandos()\y * escala / 8)
            NormalizaPunto(@LastPoint)
          Case #GR_LINE
            With graficos()\comandos()
              LineSpeccy(Int (\x * escala / 8), Int(\y * escala / 8), \over, \inverse, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, @LastPoint)
            EndWith
          Case #GR_BLOCK
            With graficos()\comandos()
              BoxSpeccy(\x, \y, \ancho, \alto, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash)
            EndWith
          Case #GR_TEXT
            With graficos()\comandos()
              PrintCharSpeccy(\x, \y, \charset, \caracter1, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, \inverse, \over)
            EndWith
          Case #GR_FILL
            With graficos()\comandos()
              Shade(\x, \y, escala, -1, -1, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, \inverse, @LastPoint)
            EndWith
          Case #GR_SHADE
            With graficos()\comandos()
              Shade(\x, \y, escala, \caracter1, \caracter2, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash, \inverse, @LastPoint)
            EndWith
          Case #GR_INK
            ActualColor\ink = graficos()\comandos()\valor
          Case #GR_PAPER
            ActualColor\paper = graficos()\comandos()\valor
          Case #GR_BRIGHT
            ActualColor\bright = graficos()\comandos()\valor
          Case #GR_FLASH
            ActualColor\flash = graficos()\comandos()\valor
          Case #GR_GOSUB
            If ListSize(pila_subrutinas()) <=10
              AddElement(pila_subrutinas())
              pila_subrutinas()\escala = escala
              pila_subrutinas()\grafico = numero_grafico
              pila_subrutinas()\step_retorno = ListIndex(graficos()\comandos()) + 1
              ProcesaGraficos(graficos()\comandos()\subrutina, -1, graficos()\comandos()\escala, ActualColor\ink, ActualColor\paper, ActualColor\bright, ActualColor\flash)
              SelectElement(graficos(),numero_grafico)
            EndIf
          Case #GR_End
            If ListSize(pila_subrutinas()) > 0
              LastElement(pila_subrutinas())
              DeleteElement(pila_subrutinas())
            EndIf
            ProcedureReturn
        EndSelect
      Else
        Break
      EndIf
    Next g
  Else
    MessageRequester("Wrong graphic selected", "The graphic selected doesn't exist", #PB_MessageRequester_Ok)
  EndIf
EndProcedure

Procedure ListViewClick(EventType)
  Select EventType
    Case #PB_EventType_LeftClick
      ;selecciona para ver parametros y fin de dibujo a dibujar
      StepSelected = GetGadgetState(ListView_GraphicSteps)
      ClearList(pila_subrutinas())
      ProcesaGraficos(GraphicSelected, StepSelected,0,0,0,0,0)
      GrafEditorInk = ActualColor\ink
      GrafEditorPaper = ActualColor\paper
      GrafEditorBright = ActualColor\bright
      GrafEditorFlash = ActualColor\flash
      UpdatePaletaSelector()
      ScreenToWindow()
;     Case #PB_EventType_LeftDoubleClick
;       ;selecciona y marca punto de fin de dibujado
;       StepSelected = GetGadgetState(ListView_GraphicSteps)
  EndSelect
EndProcedure

Procedure UpdateGrAttrInfo(atributo)
  info$ = ""
  SetMenuItemState(0,#Mn_make_subrutine,0)
  If atributo & %10000000 = 0
    info$ + "[Subrutine] "
    SetMenuItemState(0,#Mn_make_subrutine,1)
  EndIf
  For g=#Mn_InkBlack To #Mn_InkWhite
    SetMenuItemState(0,g,0)
  Next g
  For g=#Mn_PaperBlack To #Mn_PaperWhite
    SetMenuItemState(0,g,0)
  Next g
  attrpaper = (atributo & %00111000) >> 3
  attrbit6 = (atributo & %01000000) >> 6
  attrink = atributo & %00000111
  SetMenuItemState(0,#Mn_InkBlack+attrink,1)
  SetMenuItemState(0,#Mn_PaperBlack+attrpaper,1)
  SetGadgetText(Text_GraphicInfo, info$ + "Ink: " + StrU(attrink) + " - Paper: " + StrU(attrpaper) + " - Bit6: " + StrU(attrbit6))
EndProcedure

Procedure DefaultGrColor(Event)
  Select Event
    Case #Mn_InkBlack To #Mn_InkWhite
      selected = Event - #Mn_InkBlack
      nuevoattr = (graficos()\atributo_inicial & %11111000) | selected
      If graficos()\atributo_inicial <> nuevoattr
        graficos()\atributo_inicial = nuevoattr
        UpdateGrAttrInfo(nuevoattr)
        ProcesaGraficos(GraphicSelected, -1, 0,0,0,0,0)
        ScreenToWindow()
      EndIf
      
    Case #Mn_PaperBlack To #Mn_PaperWhite
      selected = Event - #Mn_PaperBlack
      nuevoattr = (graficos()\atributo_inicial & %11000111) | (selected << 3)
      If graficos()\atributo_inicial <> nuevoattr
        graficos()\atributo_inicial = nuevoattr
        UpdateGrAttrInfo(nuevoattr)
        ProcesaGraficos(GraphicSelected, -1, 0,0,0,0,0)
        ScreenToWindow()
      EndIf
        
    Case #Mn_make_subrutine
      valor = GetMenuItemState(0,#Mn_make_subrutine)
      graficos()\atributo_inicial = (graficos()\atributo_inicial & %01111111) | (valor << 7)
      UpdateGrAttrInfo(graficos()\atributo_inicial)
      ProcesaGraficos(GraphicSelected, -1, 0,0,0,0,0)
      ScreenToWindow()
  EndSelect
EndProcedure

Procedure Combo_Click(EventType)
  If EventType = #PB_EventType_Change
    Select EventGadget()
      Case Combo_Font
        CharsetSelected = GetGadgetState(Combo_Font) + 1
        UpdateFonts()
        If AsciiSelected >= 32 And AsciiSelected<=127
          LoadCharToEditor(CharsetSelected, AsciiSelected)
        EndIf
      Case Combo_Graphic
        GraphicSelected = GetGadgetState(Combo_Graphic)
        If GraphicSelected >= 0
          SelectElement(graficos(), GraphicSelected)
          UpdateGrAttrInfo(graficos()\atributo_inicial)
          UpdateGraphicList(GraphicSelected)
          SetGadgetState(ListView_GraphicSteps, ListSize(graficos()\comandos()))
          ClearList(pila_subrutinas())
          ProcesaGraficos(GraphicSelected, -1, 0,0,0,0,0)
          ScreenToWindow()
          GrafEditorInk = ActualColor\ink
          GrafEditorPaper = ActualColor\paper
          GrafEditorBright = ActualColor\bright
          GrafEditorFlash = ActualColor\flash
          UpdatePaletaSelector()
        Else
          SetGadgetText(Text_GraphicInfo,"")
          ClsSpeccy(56)
          ScreenToWindow()
        EndIf
    EndSelect
  EndIf
EndProcedure

Procedure LoadFromFile(puntero, tamano)
  Shared ActualColor
  
  maximo = puntero + tamano
  ;lee las estructuras que encuentre en el bloque de memoria
  leida.String
  While puntero < maximo
    puntero = trimspace(puntero,maximo)
    puntero = getpalabra(puntero,maximo, @leida,"{")
    Select UCase(leida\s)
      Case "CHARACTERS"
        puntero = CargaCaracteres(puntero,maximo,udgs(),shades(),fonts())
        If puntero = 0
          ProcedureReturn 0
        Else
          ClearGadgetItems(Combo_Font)
          For g = 1 To Int(ListSize(fonts())/768)
            AddGadgetItem(Combo_Font,g - 1,"Charset " + StrU(g))
          Next g
          SetGadgetState(Combo_Font, 0)
        EndIf
      Case "GRAPHICS"
        puntero = CargaGraficos(puntero, maximo, graficos())
        If puntero = 0
          ProcedureReturn 0
        Else
          ParseaGraficos(graficos())
          ClearGadgetItems(Combo_Graphic)
          For g = 0 To ListSize(graficos()) - 1
            AddGadgetItem(Combo_Graphic, g, "Graphic " + StrU(g))
          Next g
          SetGadgetState(Combo_Graphic, 0)
          UpdateGraphicList(0)
          ProcesaGraficos(0,-1,0,0,0,0,0)
          GrafEditorInk = ActualColor\ink
          GrafEditorPaper = ActualColor\paper
          GrafEditorBright = ActualColor\bright
          GrafEditorFlash = ActualColor\flash
          UpdatePaletaSelector()
          ScreenToWindow()
        EndIf
    EndSelect
  Wend
  ProcedureReturn 1
EndProcedure

Procedure AbrirArchivo(Event)
  Shared in_archivo$
  in_archivo$=OpenFileRequester("Select file to open","","Paws Gfx|*.gfx|Paws Charset|*.ec|Paws Graphics|*.eg",0)
  If in_archivo$ <> ""
    in_file = ReadFile(#PB_Any, in_archivo$)
    If in_file
      tamano = Lof(in_file)
      memoria = AllocateMemory(tamano)
      If memoria
        If ReadData(in_file,memoria,tamano)
          CloseFile(in_file)
          If LoadFromFile(memoria, tamano) <> 0
            CharsetSelected = 0
            AsciiSelected = 144
            UpdateUdgs()
            UpdateShades()
            If ListSize(fonts()) >= 768
              CharsetSelected = 1
              AsciiSelected = 33
            EndIf
            UpdateFonts()
            LoadCharToEditor(CharsetSelected,AsciiSelected)
            FreeMemory(memoria)
            SetWindowTitle(Window_0, "Paws GFX Editor - " + in_archivo$)
          EndIf
        Else
          MessageRequester("Error","Error opening the file", #PB_MessageRequester_Ok)
        EndIf
      EndIf
    Else
      MessageRequester("Error","Error opening the file", #PB_MessageRequester_Ok)
    EndIf
  EndIf
EndProcedure

Procedure WorkOnScreen(EventType)
  Select EventType
    Case #PB_EventType_MouseMove
      x = Int(GetGadgetAttribute(Canvas_Screen, #PB_Canvas_MouseX) / 2)
      y = 175 - Int(GetGadgetAttribute(Canvas_Screen, #PB_Canvas_MouseY) / 2)
      SetGadgetText(Text_InfoMouse,"X: " + RSet(StrU(x),3,"0") + " - Y: "+RSet(StrU(y),3,"0"))
    Case #PB_EventType_LeftClick
      x = Int(GetGadgetAttribute(Canvas_Screen, #PB_Canvas_MouseX) / 2)
      y = 175 - Int(GetGadgetAttribute(Canvas_Screen, #PB_Canvas_MouseY) / 2)
      ;PlotSpeccy(x, y, GrafEditorOver, GrafEditorInverse, GrafEditorInk, GrafEditorPaper, GrafEditorBright, GrafEditorFlash, @LastPoint)
      LineSpeccy(x-LastPoint\x, y-LastPoint\y, GrafEditorOver, GrafEditorInverse, GrafEditorInk, GrafEditorPaper, GrafEditorBright, GrafEditorFlash, @LastPoint)
      ScreenToWindow()
  EndSelect
EndProcedure


Restore por_defecto
;creamos las estructuras por defecto
For g = 0 To 151
  AddElement(udgs())
  Read.a udgs()
Next g

For g = 152 To 279
  AddElement(shades())
  Read.a shades()
Next g
For g = 0 To 767 + 128   ;se cargan a partir de chr 128 hasta chr 143 las definiciones de los cuadrados
  AddElement(font_speccy())
  Read.a font_speccy()
Next g

ImagenEditor = CreateImage(#PB_Any,8,8)
numcharsets = 0

;ahora creamos la paleta básica del Spectrum
For g = 0 To 7
  rojo = (g & %010) >> 1
  verde =  (g & % 100) >> 2
  azul =  g & % 001
  paleta(g,0) = RGB(rojo * 213, verde * 213, azul * 213)
  paleta(g,1) = RGB(rojo * 255, verde * 255, azul * 255)
Next g

;dejamos los atributos a 56 (paper 7, ink 0)
GrafEditorInk = 0
GrafEditorPaper = 7
GrafEditorBright = 0
GrafEditorFlash = 0
GrafEditorOver = 0
GrafEditorInverse = 0
LastPoint\x = 0
LastPoint\y = 0

For x = 0 To 31
  For y = 0 To 21
    SpectrumAttr(x,y) = GrafEditorInk + GrafEditorPaper * 8 + GrafEditorBright * 64
  Next y
Next x


ImagenScreen = CreateImage(#PB_Any, 256, 176)

main:
OpenWindow_0()
SetGadgetAttribute(Canvas_Screen,#PB_Canvas_Cursor, #PB_Cursor_Cross)
AddWindowTimer(Window_0, 1,320)
ScreenToWindow()
UpdatePaletaSelector()


;iniciamos todos los elementos de la ventana
UpdateUdgs()
UpdateShades()
CharsetSelected = 0
AsciiSelected = 0
LoadCharToEditor(0,0)
;los siguientes menus estan deshabilitados, si no hay charset no tienen sentido
DisableMenuItem(0,#Mn_delete_charset,1)
DisableMenuItem(0,#Mn_clear_charset,1)
DisableMenuItem(0,#Mn_exp_font,1)


Repeat
  Event = WaitWindowEvent()
  
  Select EventWindow()
    Case Window_0
      If Event <> #PB_Event_Timer
        Window_0_Events(Event)
      Else
        InvertedFlash = 1 - InvertedFlash
        UpdateFlash()
      EndIf
  EndSelect
  
Until Event = #PB_Event_CloseWindow



DataSection
  por_defecto:
  IncludeBinary "udg_shades.bin"
  IncludeBinary "font_speccy.bin" ;este font es el de la rom
  IncludeBinary "squares.bin" ;añadiendo las definiciones de los chr 128 - 143
  font_default:
  IncludeBinary "font_default.bin"
  paleta:
  Data.a 0,0,0,0,0,203,203,0,0,203,0,203,0,203,0,0,203,203,203,203,0,203,203,203,0,0,0,0,0,255,255,0,0,255,0,255,0,255,0,0,255,255,255,255,0,255,255,255
EndDataSection

; IDE Options = PureBasic 5.31 (Windows - x86)
; CursorPosition = 751
; FirstLine = 164
; Folding = AAAQAAAg
; EnableUnicode
; Executable = PawsGFX.exe