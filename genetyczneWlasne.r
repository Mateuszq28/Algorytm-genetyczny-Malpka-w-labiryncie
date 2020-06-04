source('gra.r')
fitness <-function (obiekt) 
{
  return (gra(obiekt));
}

wyswietlac=TRUE

liczbaobiektow=80
liczbagenow=22
liczbageneracji=1000
obiekty=matrix(nrow=liczbaobiektow, ncol=liczbagenow) #populacja, uwaga na typy zmiennych

#losowa inicjalizacja populacji
set.seed(as.integer(Sys.time()))
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
        if(runif(1)<=0.7)
          obiekty[i,j]=obiekty[best,j]			

        #Mutacja losowa
        if(runif(1)<=0.3) #ten zapis oznacza, ze XX genow sa zmieniane
        obiekty[i,j]=floor(4*runif(1))+1
      }
    }
    if(wyswietlac) print(obiekty[i,])
  }
  
  if(wyswietlac) print(sprintf("Fitness: %d",fitness(obiekty[best,])))
  if (fitness(obiekty[best,])>=150)
    {
    print(sprintf("generacja %d",g))
    #print(obiekty)
    print(best)
    print(obiekty[best,])
    break
    }
}
