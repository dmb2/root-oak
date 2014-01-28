{
  TH1F h1("h1","hist",5,-5,5);
  TH1F h2("h2","hist",5,-5,5);
  TH2F HistTwoD("HistTwoD","2D Histogram",5,-5,5, 5,-5,5);
  h1.FillRandom("gaus",50000);
  h2.FillRandom("gaus",50000);
  HistTwoD.Multiply(&h1,&h2);
  HistTwoD.Draw();
      
}
