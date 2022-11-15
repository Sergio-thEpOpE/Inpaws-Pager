import sys

# El script require al menos un parámetro, el fichero del que extraer los datos.
# La salida será por la ventana de la consola, salvo que se especifique un segundo parámetro
# como fichero de salida, al que se redigirá.

if len(sys.argv)>1:
	in_file=sys.argv[1]
	a=open(in_file,'rb')
	if len(sys.argv)>2:
		out_file=sys.argv[2]
		b=open(out_file,'w')
		sys.stdout=b
else:
	print('Especifique un fichero de entrada.')
	exit()

longitud=len(a.read())
print('Longitud=',longitud)

offset=65536-longitud
print('offset=',offset)

a.seek(longitud-3)
graficos=int.from_bytes(a.read(1),byteorder='little')
print('Número de gráficos=', graficos)

a.seek(longitud-7)
colores=int.from_bytes(a.read(2),byteorder='little')
off_colores=colores-offset
print('Tabla de colores en:', colores)

a.seek(longitud-9)
shades=int.from_bytes(a.read(2),byteorder='little')
off_shades=shades-offset
print('Juego de caractéres y shades en:', shades)

a.seek(longitud-11)
fin_ventanas=int.from_bytes(a.read(2),byteorder='little')
off_fin_ventanas=fin_ventanas-offset
print('Final de la tabla de ventanas en:', fin_ventanas)

a.seek(longitud-13)
ventanas=int.from_bytes(a.read(2),byteorder='little')
off_ventanas=ventanas-offset
print('Tabla de ventanas en:', ventanas)

a.seek(longitud-15)
punteros=int.from_bytes(a.read(2),byteorder='little')
off_punteros=punteros-offset
print('Tabla de punteros en:', punteros)

a.seek(longitud-17)
inicio=int.from_bytes(a.read(2),byteorder='little')
off_inicio=inicio-offset
print('Gráfico 0 o dirección de inicio en:', inicio)

a.seek(longitud-19)
spare=int.from_bytes(a.read(2),byteorder='little')
print('Valor actual de spare: ', spare)

print()
print('TABLA DE COLORES')
a.seek(off_colores)
print()
print('BORDER:',int.from_bytes(a.read(1),byteorder='little'))
for x in range(1,9):
	print(x,int.from_bytes(a.read(1),byteorder='little'))

#Tabla de VENTANAS

windows=[]
a.seek(off_ventanas)
for x in range(0,graficos):
	#print()
	#print(format(x,'02'))
	y=int.from_bytes(a.read(1),byteorder='little')
	if y<128:
		#print('SUBRUTINA')
		windows.append([y,0,0,0,0])
		a.seek(4,1)
	else:
		aux=[]
		aux.append(y)
		y=y-128
		paper=y//8
		ink=y%8
		#print('Paper',paper,'- Ink',ink)
		y=int.from_bytes(a.read(1),byteorder='little')
		aux.append(y)
		#print('Línea:',y)
		y=int.from_bytes(a.read(1),byteorder='little')
		aux.append(y)
		#print('Columna',y)
		y=int.from_bytes(a.read(1),byteorder='little')
		aux.append(y)
		#print('Alto:',y)
		y=int.from_bytes(a.read(1),byteorder='little')
		aux.append(y)
		#print('Ancho',y)
		windows.append(aux)


# Tabla de PUNTEROS		
		
zx_address=[]
abs_address=[]
a.seek(off_punteros)
for x in range(0,graficos):
	y=int.from_bytes(a.read(2),byteorder='little')
	zx_address.append(y)
	abs_address.append(y-offset)
	
# Formato del diccionario d
# -Key: Código del comando (0-255)
# -Una lista de 5 elemantos:
# 0:Cádena de texto con el nombre impreso del comando
# 1:Nº de parámetros que el comando usa (0-4)
# 2:1 si el comando usa coordenada x negativa, 0 en caso contrario (usa x positiva o simplemente no es aplicable)
# 3:1 si el comando usa coordenada y negativa, 0 en caso contrario (usa y positiva o simplemente no es aplicable)
# 4:1 si el comando comprime las coordenadas x e y en un sólo byte (fórmula x*16+y), 0 si no lo hace o no es aplicable	
	
d={
0:['PLOT',2,0,0,0],
1:['LINE',2,0,0,0],
2:['FILL',2,0,0,0],
3:['GOSUB scale 0',1,0,0,0],
4:['TEXT',3,0,0,0],
5:['PAPER 0',0,0,0,0],
6:['INK 0',0,0,0,0],
7:['FIN de GRÁFICO',0,0,0,0],
8:['PLOT over',2,0,0,0],
9:['LINE over',2,0,0,0],
11:['GOSUB scale 1',1,0,0,0],
12:['TEXT over',3,0,0,0],
13:['PAPER 1',0,0,0,0],
14:['INK 1',0,0,0,0],
16:['PLOT inverse',2,0,0,0],
17:['LINE inverse',2,0,0,0],
18:['BLOCK',4,0,0,0],
19:['GOSUB scale 2',1,0,0,0],
20:['TEXT inverse',3,0,0,0],
21:['PAPER 2',0,0,0,0],
22:['INK 2',0,0,0,0],
24:['ABS MOVE',2,0,0,0],
25:['REL MOVE',2,0,0,0],
27:['GOSUB scale 3',1,0,0,0],
28:['TEXT inverse over',3,0,0,0],
29:['PAPER 3',0,0,0,0],
30:['INK 3',0,0,0,0],
33:['LINE',1,0,0,1],
34:['SHADE',3,0,0,0],
35:['GOSUB scale 4',1,0,0,0],
37:['PAPER 4',0,0,0,0],
38:['INK 4',0,0,0,0],
41:['LINE over',1,0,0,1],
43:['GOSUB scale 5',1,0,0,0],
45:['PAPER 5',0,0,0,0],
46:['INK 5',0,0,0,0],
49:['LINE inverse',1,0,0,1],
50:['SHADE inverse',3,0,0,0],
51:['GOSUB scale 6',1,0,0,0],
53:['PAPER 6',0,0,0,0],
54:['INK 6',0,0,0,0],
57:['REL MOVE',1,0,0,1],
59:['GOSUB scale 7',1,0,0,0],
61:['PAPER 7',0,0,0,0],
62:['INK 7',0,0,0,0],
65:['LINE',2,1,0,0],	
66:['FILL',2,1,0,0],
67:['GOSUB scale 0 rotate x',1,0,0,0],
69:['PAPER 8',0,0,0,0],
70:['INK 8',0,0,0,0],
73:['LINE over',2,1,0,0],
75:['GOSUB scale 1 rotate x',1,0,0,0],
81:['LINE inverse',2,1,0,0],
83:['GOSUB scale 2 rotate x',1,0,0,0],
89:['REL MOVE',2,1,0,0],
91:['GOSUB scale 3 rotate x',1,0,0,0],
97:['LINE',1,1,0,1],
98:['SHADE',3,1,0,0],
99:['GOSUB scale 4 rotate x',1,0,0,0],
105:['LINE over',1,0,0,1],
107:['GOSUB scale 5 rotate x',1,0,0,0],
113:['LINE inverse',1,0,0,1],
114:['SHADE inverse',3,1,0,0],
115:['GOSUB scale 6 rotate x',1,0,0,0],
121:['REL MOVE',1,1,0,1],
123:['GOSUB scale 7 rotate x',1,0,0,0],			
129:['LINE',2,0,1,0], 					
130:['FILL',2,0,1,0],
131:['GOSUB scale 0 rotate y',1,0,0,0],				
133:['BRIGHT 0',0,0,0,0],
134:['FLASH 0',0,0,0,0],
137:['LINE over',2,0,1,0],
139:['GOSUB scale 1 rotate y',1,0,0,0],			
141:['BRIGHT 1',0,0,0,0],
142:['FLASH 1',0,0,0,0],
145:['LINE inverse',2,0,1,0],
147:['GOSUB scale 2 rotate y',1,0,0,0], 			
153:['REL MOVE',2,0,1,0],
155:['GOSUB scale 3 rotate y',1,0,0,0],
161:['LINE',1,0,1,1],	
162:['SHADE',3,0,1,0],
163:['GOSUB scale 4 rotate y',1,0,0,0],
169:['LINE over',1,0,1,1],
171:['GOSUB scale 5 rotate y',1,0,0,0],
177:['LINE inverse',1,0,1,1],			
178:['SHADE inverse',3,0,1,0],
179:['GOSUB scale 6 rotate y',1,0,0,0],
185:['REL MOVE',1,0,1,1],
187:['GOSUB scale 7 rotate y',1,0,0,0],			
193:['LINE',2,1,1,0], 				
194:['FILL',2,1,1,0],
195:['GOSUB scale 0 rotate x rotate y',1,0,0,0],		
197:['BRIGHT 8',0,0,0,0],
198:['FLASH 8',0,0,0,0],
201:['LINE over',2,1,1,0],
203:['GOSUB scale 1 rotate x rotate y',1,0,0,0],		
209:['LINE inverse',2,1,1,0],
211:['GOSUB scale 2 rotate x rotate y',1,0,0,0], 		
217:['REL MOVE',2,1,1,0],
219:['GOSUB scale 3 rotate x rotate y',1,0,0,0],
225:['LINE',1,1,1,1],					
226:['SHADE',3,1,1,0],
227:['GOSUB scale 4 rotate x rotate y',1,0,0,0],
233:['LINE over',1,1,1,1],
235:['GOSUB scale 5 rotate x rotate y',1,0,0,0],
241:['LINE inverse',1,1,1,1],		
242:['SHADE inverse',3,1,1,0],
243:['GOSUB scale 6 rotate x rotate y',1,0,0,0],
249:['REL MOVE',1,1,1,1],
251:['GOSUB scale 7 rotate x rotate y',1,0,0,0]
}

print()
print('GRAFICOS')
print()

for x in range(0,graficos):
	a.seek(abs_address[x])
	y=0
	print ('** GRÁFICO',x,'**')
	print ('Dirección: ',a.tell()+offset)
	if windows[x][0]<128:
		print('SUBRUTINA')
	else:
		t=windows[x][0]-128
		paper=t//8
		ink=t%8
		print('Paper:',paper,'Ink:',ink,'Línea:',windows[x][1],'Columna:',windows[x][2],'Alto',windows[x][3],'Ancho',windows[x][4])
	print()
	while y !=7:
		w=a.tell()+offset
		print(w,end=' ')
		y=int.from_bytes(a.read(1),byteorder='little')
		if y in d:
			cadena=''
			print (format(y,'3'),end=' ')
			cadena=cadena+d[y][0]+' '
			for z in range(0,d[y][1]):
				v=int.from_bytes(a.read(1),byteorder='little')
				if d[y][4]==1:
					print(format(v,'3'),end=' ')
					v1=int(v/16)
					v2=v%16
					if d[y][2]==1:
						cadena=cadena+'-'
					cadena=cadena+str(v1)+' '
					if d[y][3]==1:
						cadena=cadena+'-'
					cadena=cadena+str(v2)+' '
				else:	
					if z==0 and d[y][2]==1:
						cadena=cadena+'-'
					if z==1 and d[y][3]==1:
						cadena=cadena+'-'
					print(format(v,'3'), end=' ')
					cadena=cadena+str(v)+' '
		else:
			print(a.tell(),'WARNING, unknown command.',y,'en',a.tell(), a.tell()+offset-1, end=' ')
			v=int.from_bytes(a.read(1),byteorder='little')
			print(v)
		espacios=18-(4*d[y][1])
		print(espacios*' '+'- '+cadena)
	print()
	
a.close()
if sys.stdout != sys.__stdout__:
	b.close()
