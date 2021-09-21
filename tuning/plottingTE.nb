meantone = {{1, 2, 4}, {0, -1, -4}};
M = meantone.tenneyWeights[meantone];
G = {x, y};
V = {1, 1, 1};


Plot3D[
  Transpose[(G.M - V)].(G.M - V),
  {x, 0.99, 1.01},
  {y, 0.41, 0.43},
  PlotTheme -> "Web"
]
