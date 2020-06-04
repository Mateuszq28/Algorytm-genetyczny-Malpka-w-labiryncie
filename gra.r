mapa = matrix( rbind( c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
                      c('#', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', 'D', ' ', '#'),
                      c('#', ' ', 'D', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'),
                      c('#', ' ', ' ', ' ', '#', ' ', ' ', ' ', '#', ' ', ' ', 'M', '#'),
                      c('#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', '#'),
                      c('#', ' ', ' ', ' ', ' ', ' ', 'D', ' ', '#', 'D', ' ', ' ', '#'),
                      c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')), ncol=13, nrow=7)
MAXruchow = 22
wyswietlanieKroku = FALSE
wyswietlanieKoncaGry = FALSE

wyswietlGracza <-function(mapa, pozycja)
{
  mapaBuf = mapa
  mapaBuf[pozycja[1], pozycja[2]] = '@'
  print(mapaBuf)
}

gra <-function (wektorRuchu)
{
  pozycja = c(4, 2)
  punkty = 0
  diamenty = 0
  mapaDyn = mapa
  
  if (wyswietlanieKroku) print(mapaDyn)
  
  for (r in c(1:MAXruchow))
  {
     punkty = MAXruchow - r
     if (wyswietlanieKroku) wyswietlGracza(mapaDyn, pozycja)
     #zmiana pozycji ma³py
     if (wektorRuchu[r]==1) pozycja[1] = pozycja[1]-1
     else if (wektorRuchu[r]==2) pozycja[2] = pozycja[2]+1
     else if (wektorRuchu[r]==3) pozycja[1] = pozycja[1]+1
     else if (wektorRuchu[r]==4) pozycja[2] = pozycja[2]-1
     #sprawdzanie czy ma³pa nie trafi³a na pole specjalne
     if (mapaDyn[pozycja[1], pozycja[2]] == 'M') break
     else if (mapaDyn[pozycja[1], pozycja[2]] == '#') break
     else if (mapaDyn[pozycja[1], pozycja[2]] == 'D')
     {
       diamenty = diamenty+1
       mapaDyn[pozycja[1], pozycja[2]] = ' '
     }
  }
  
  #liczenie punktów
  punkty = punkty + 3*diamenty + 10 - (12-pozycja[2])
  if (mapaDyn[pozycja[1], pozycja[2]] == 'M') punkty = punkty + 150
  else if (pozycja[2]>=9) punkty = punkty + 100
  else if (pozycja[2]>=5) punkty = punkty + 50
    
  if (wyswietlanieKoncaGry) wyswietlGracza(mapaDyn, pozycja)
  
  return(punkty);
}

#Testowanie najlepszego przypadku w obecnoœci diamentów (171 punktów, 20 potrzebnych ruchów)
#uwaga! wektor nie musi byæ dope³niony do 22, wystarczy 20 pozycji
#gra(c(2,1,3,3,3,2,2,2,2,1,1,1,2,2,2,2,1,3,3,2))
#Testowanie najlepszego przypadku bez diamentów (168 punktów, 14 potrzebnych ruchów)
#gra(c(2,2,3,2,2,2,2,1,1,2,2,2,2,3))