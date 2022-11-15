# dump.py es un script auxiliar. Su misión es hacer un volcado "tal cual" de las posiciones de memoria
# de un fichero de gráficos. Es útil para contrastar durante el proceso de investigación creando bases
# de datos gráficas pequeñitas y viendo que su contenido cuadra con el output del extractor.

import sys

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
end=65536
start=end-longitud

a.seek(0)

for x in range(start,end):
	y=a.read(1)
	z=int.from_bytes(y,byteorder='little')
	print(x,format(x,'X'),format(z,'02X'),format(z,' 4'))
	
a.close()
if sys.stdout != sys.__stdout__:
	b.close()
