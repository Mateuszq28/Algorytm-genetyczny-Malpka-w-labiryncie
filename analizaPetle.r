library('plot.matrix')

#potrzebne do liczenia œredniej liczby potrzebnych generacji
liczbaTestow=100
sumaGeneracji=0
#wyœwietlanie
wyswietlacTesty=FALSE
wyswietlacWynikiTestow=FALSE
#zapisane wyniki z kazdego testu
wynikiTestow=matrix(ncol=liczbaTestow)

#do szukania optymalnych prawdopodobieñstw
LICZBApARAM=13
prawdopMutacji= c(0, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 1)
prawdopKrzyz= c(0, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 1)
najlepszaPara = c(1,1)
#zapisane œrednie liczby potrzebnych generacji dla rownych prawdopodobienstw mutacji i krzyzowania
wynikiDlaParametrow=matrix(nrow=LICZBApARAM, ncol=LICZBApARAM)


fitness <-function (obiekt) 
{
  #tu zawarte jest wlasciwe kodowanie genu i jego ocena
  #kod genu, zawartosc skladnikow w recepcje na sok cytrynowy
  #[1]: sol
  #[2]: cukier
  #[3]: cytryna
  #[4]: jajko
  #[5]: woda
  #[6]: cebula
  #[7]: jablko
  #wlasciwy przepis - sol + cukier + cytryna - jajko + woda - cebula - jablko
  #jezeli odpowiednie geny w chromosomie beda mialy wartosc 0 a inne 1, to najlepsze
  #dopasowanie do tej reguly, da wynik funkcji oceny = 3 (dlaczego?)
  ocena = -obiekt[1] + obiekt[2] + obiekt[3] - obiekt[4] + obiekt[5] - obiekt[6]- obiekt[7]
  #ocena dopasowania przepisu do rzeczywystej recepty:
  return (ocena);
}

wyswietlac=FALSE

liczbaobiektow=3
liczbagenow=7
liczbageneracji=100
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
      
      for(i in c(1:liczbaobiektow))
      {
        #wype-nij losowymi wartosciami
        obiekty[i,] = (runif(liczbagenow)<=0.5)*1
        
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
              if(runif(1)<=prawdopMutacji[mutTest]) #ten zapis oznacza, ze 1/2 genow sa zmieniane
                obiekty[i,j]=(runif(1)<=0.5)*1
            }
          }
          if(wyswietlac) print(obiekty[i,])
        }
        
        if(wyswietlac) print(sprintf("Fitness: %d",fitness(obiekty[best,])))
        if (fitness(obiekty[best,])==3)
        {
          if (wyswietlacTesty)
          {
            print(sprintf("generacja %d",g))
            print(obiekty)
            print(best)
          }
          break
        }
      }
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