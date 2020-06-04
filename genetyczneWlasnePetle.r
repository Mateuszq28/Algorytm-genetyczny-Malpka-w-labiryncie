library('plot.matrix')

#potrzebne do liczenia œredniej liczby potrzebnych generacji
liczbaTestow=10
sumaGeneracji=0
#wyœwietlanie
wyswietlacTesty=FALSE
wyswietlacWynikiTestow=FALSE
#zapisane wyniki z kazdego testu
wynikiTestow=matrix(ncol=liczbaTestow)

#do szukania optymalnych prawdopodobieñstw
LICZBApARAM=5
prawdopMutacji= c(0.1, 0.3, 0.5, 0.7, 0.9)
prawdopKrzyz= c(0.1, 0.3, 0.5, 0.7, 0.9)
najlepszaPara = c(1,1)
#zapisane œrednie liczby potrzebnych generacji dla rownych prawdopodobienstw mutacji i krzyzowania
wynikiDlaParametrow=matrix(nrow=LICZBApARAM, ncol=LICZBApARAM)

source('gra.r')
fitness <-function (obiekt) 
{
  return (gra(obiekt));
}

wyswietlac=FALSE

liczbaobiektow=80
liczbagenow=22
liczbageneracji=1000
obiekty=matrix(nrow=liczbaobiektow, ncol=liczbagenow) #populacja, uwaga na typy zmiennych

#losowa inicjalizacja populacji
set.seed(as.integer(Sys.time()))

najmniejszaSrednia = liczbageneracji
for (mutTest in c(1:LICZBApARAM))
{
  for (krzyzTest in c(1:LICZBApARAM))
  {
    for (t in c(1:liczbaTestow))
    {
#*****************************************************************************
      for(i in c(1:liczbaobiektow))
      {
          #wype-nij losowymi wartosciami
        obiekty[i,] = floor(4*runif(liczbagenow))+1
      
      }
      
      for(g in c(0:liczbageneracji))
      {
        if(wyswietlac) print(sprintf("generacja %d",g))
        
        #Ocena
        best=1 #zawiera indeks wyznaczajacy pozycje najlepszego objektu w populacji
        
        for(i in c(1:liczbaobiektow))
        {
          if(fitness(obiekty[best,])<fitness(obiekty[i,]))
          {
            best=i
          }
        }
        
        
        #Reprodukcja (podejscie Elitarne) z Mutacja
        for(i in c(1:liczbaobiektow))
        {
          #nie krzyzuj najlepszego (best) sie z soba samym
          if(i!=best)
          {
            for(j in c(1:liczbagenow))
            {
              #krzyzowanie - losowo podmieniaj gen j na gen z objektu najlepszego
              if(runif(1)<=prawdopKrzyz[krzyzTest])
                obiekty[i,j]=obiekty[best,j]			
      
              #Mutacja losowa
              if(runif(1)<=prawdopMutacji[mutTest]) #ten zapis oznacza, ze XX genow sa zmieniane
              obiekty[i,j]=floor(4*runif(1))+1
            }
          }
          if(wyswietlac) print(obiekty[i,])
        }
        
        if(wyswietlac) print(sprintf("Fitness: %d",fitness(obiekty[best,])))
        if (fitness(obiekty[best,])>=150)
          {
          if (wyswietlacTesty)
          {
            print(sprintf("generacja %d",g))
            #print(obiekty)
            print(best)
            print(obiekty[best,])
          }
          break
          }
      }
      #*****************************************************************************
      wynikiTestow[t]=g
      sumaGeneracji=sumaGeneracji+g
      
    }
    
    sredniaTestow=sumaGeneracji/liczbaTestow
    wynikiDlaParametrow[mutTest,krzyzTest]=sredniaTestow
    if (wyswietlacWynikiTestow)
    {
      print("Liczby generacji, po których osi¹gniêto sukces dla kolejnych testów:")
      print(wynikiTestow)
    }
    print(sprintf("Test: %d Prawd Mut: %f Prawd Krzyz: %f Œrednia liczba potrzebnych generacji: %f",(mutTest-1)*LICZBApARAM+krzyzTest ,prawdopMutacji[mutTest], prawdopKrzyz[krzyzTest], sredniaTestow))
    if (sredniaTestow<=najmniejszaSrednia)
    {
      najmniejszaSrednia=sredniaTestow
      najlepszaPara=c(mutTest, krzyzTest)
    }
    sumaGeneracji=0
  }
}

print(sprintf("Najlepsze wyniki (%f generacji) osi¹gniêto dla Prawd Mut: %f Prawd Krzyz: %f",najmniejszaSrednia, prawdopMutacji[najlepszaPara[1]], prawdopKrzyz[najlepszaPara[2]]))
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(wynikiDlaParametrow, xlab='Index prawdopodobieñstwa mutacji', ylab='Index prawdopodobieñstwa krzy¿ówki')
      