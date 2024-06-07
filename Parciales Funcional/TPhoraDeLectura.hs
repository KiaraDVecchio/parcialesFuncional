
{-El visitante de Stephen King que tiene 592 páginas, Shingeki no Kyojin capítulo 
1, 3 y 127 que tienen 40 páginas cada uno y los escribió Hajime Isayama, 
Fundación de Isaac Asimov con 230 páginas, los capítulos  5, 10 y 12 de Sandman 
de Neil Gaiman con 35 páginas cada uno y, por último, la saga de Eragon: Eragon 
(544 páginas), Eldest (704 páginas), Brisignr (700 páginas) y Legado (811 páginas) 
de Christopher Paolini.

Estas son cosas que nos gustarían saber:
1. El promedioDeHojas de los libros de nuestra biblioteca.
2. Qué lectura es una lecturaObligatoria, esto es así cuando es de Stephen King o de la saga de Eragon 
o es el ejemplar de Fundación de 230 páginas de Isaac Asimov (¡ningún otro!).
3. Si la biblioteca es fantasiosa, es decir, si tiene algún libro de Christopher Paolini o de Neil Gaiman.
5. Si tenemos una bibliotecaLigera, o sea, si todas sus lecturas tienen 40 páginas o menos.-}



-- Definimos la Tupla (tipo de dato) de las variables que usaremos
type Titulo = String
type Autor = String
type CantidadDePaginas = Int
type Libro = (Titulo, Autor, CantidadDePaginas) 

--Definimos las funciones correspondientes a cada libro con su Titulo, Autor y Cant. de pags.
elVisitante :: Libro
elVisitante = ("El Visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("Shingeki no Kyojin 1", "Hajime Isayama", 40)

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = ("Shingeki no Kyojin 3", "Hajime Isayama", 40)

shingekiNoKyojin27 :: Libro
shingekiNoKyojin27 = ("Shingeki no Kyojin 27", "Hajime Isayama", 40)

fundacion :: Libro
fundacion = ("Fundacion", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("sandman5", "Neil Gaiman", 35)

sandman10 :: Libro
sandman10 = ("sandman10", "Neil Gaiman", 35)

sandman12 :: Libro
sandman12 = ("sandman12", "Neil Gaiman", 35)

eragon :: Libro
eragon = ("eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("eldest", "Christopher Paolini", 704)

brisignr :: Libro
brisignr = ("brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("legado", "Christopher Paolini", 811)

--Definimos la Tupla "Biblioteca" que es una lista de libros
type Biblioteca = [Libro]

--Esa biblioteca contiene todos los libros
miBiblioteca :: Biblioteca
miBiblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin3, shingekiNoKyojin27, 
        fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]


--1. Promedio de hojas de la biblioteca
cantidadDePaginas :: Libro -> Int  --Defino la funcion, le paso un libro y me devuelve un entero, que sería la cany. de pags.
cantidadDePaginas (_, _, paginas) = paginas 

--Llama a la funcion cantidadDePaginas de cada libro y devuelve una lista. Luego suma las paginas de todos los libros
cantidadDePaginasTotales :: Biblioteca -> Int
cantidadDePaginasTotales unaBiblioteca = sum (map cantidadDePaginas unaBiblioteca) 

cantidadDePaginasTotales' :: Biblioteca -> Int
cantidadDePaginasTotales' unaBiblioteca = sum . map cantidadDePaginas $ unaBiblioteca

--Recibo una biblioteca, divido toda la biblioteca por la cantidad de hojas totales
promedioDePaginas :: Biblioteca -> Int
promedioDePaginas unaBiblioteca = div (cantidadDePaginasTotales unaBiblioteca) (length unaBiblioteca) --lenght: Cant de elementos que tiene la lista


--2. Lectura Obligatoria

--Define el tipo Saga que es una lista de libros
type Saga = [Libro]

--Recibe un libro y me devuelve su autor
autor :: Libro -> String
autor (_, autor, _) = autor

--Recibe un libro y me devuelve si SI (True) es obligatoria o si NO lo es (False)
esLecturaObligatoria :: Libro -> Bool
esLecturaObligatoria unLibro = esDeStephenKing unLibro || perteneceASagaEragon unLibro || esFundacion unLibro

--Composicion de funciones: leo de atras hacia adelante. Recibe un libro, lee el autor y verifica que 
--el autor es Stephen King. Si es asi, devuelve TRUE 
esDeStephenKing :: Libro -> Bool
esDeStephenKing unLibro = ((== "Stephen King") . autor) unLibro

perteneceASagaEragon :: Libro -> Bool
perteneceASagaEragon unLibro = elem unLibro sagaDeEragon

sagaDeEragon :: Saga
sagaDeEragon = [eragon, eldest, brisignr, legado]

esFundacion :: Libro -> Bool
esFundacion unLibro = unLibro == fundacion











