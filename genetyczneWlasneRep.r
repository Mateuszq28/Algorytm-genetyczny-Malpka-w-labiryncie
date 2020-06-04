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
oceny=c(ncol=liczbagenow)

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
    oceny[i] = fitness(obiekty[i,])
    if(fitness(obiekty[best,])<oceny[i])
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
      #jeœli du¿o punktów, to znaczy, ¿e pocz¹tek jest dobrze
      zacznij=1
      if (oceny[i]>=100) zacznij=8
      else if (oceny[i]>=50) zacznij=4
      
      for(j in c(zacznij:liczbagenow))
      {
        #krzyzowanie - losowo podmieniaj gen j na gen z objektu najlepszego
        if(runif(1)<=0.1)
        {
          obiekty[i,j]=obiekty[best,j]
          if (j+1 <= liczbagenow && runif(1)<=0.2)
          obiekty[i,j+1]=obiekty[best,j+1]
        }
          			
        #Mutacja losowa
        if(runif(1)<=0.1) #ten zapis oznacza, ze XX genow sa zmieniane
        {
          if (j+1 <= liczbagenow && runif(1)<=0.2)
            obiekty[i,j] = obiekty[i,j+1]
          else
            obiekty[i,j]=floor(4*runif(1))+1
        }
        
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
