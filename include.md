### About

This is a [Shiny App](https://shiny.rstudio.com/) written to simplify the process of river survey postprocessing.

When you you upload a [**.sl2**](https://wiki.openstreetmap.org/wiki/SL2), **.sl3** or **.slg** files derived from [LOWRANCE©](https://www.lowrance.com/) Fish Finders a conversion from binary format to table will be done. This is based on [`arabia`](https://gitlab.com/hrbrmstr/arabia) package kindly created by @hbrmstr. Then depth in **feets** will be converted to **meters** and Mercator Meters to Geographic Coordinates (read more [here](http://www.oziexplorer3.com/eng/eagle.html)). Finally this app takes the **median** depth in the point and keeps one entry for every coordinate.