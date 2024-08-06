Calculate the alpha frequency from a fasta file
================

## Calculate the Amino Acids Frequency From the Human Protein Sequence

``` r
library(cppTools)
```

The Amino Acids Frequency can be calculated using the alpha_count
function.

``` r
system.time(
dfExAAsForHuman <- alpha_count(r"(GCF_000001405.40_GRCh38.p14_protein.faa)",
            seq_min = 100, 
            seq_max = 2000,
            verbose = F, simple = F
            )
)
```

    ## 用户 系统 流逝 
    ## 0.32 0.08 0.41

``` r
dfExAAsForHuman |> 
        d=>d[order(-d$Freq), ] |> 
        knitr::kable()
```

|     |    Freq |      Prop |
|:----|--------:|----------:|
| L   | 7307961 | 0.0974214 |
| S   | 6452376 | 0.0860157 |
| E   | 5457680 | 0.0727556 |
| A   | 5094653 | 0.0679161 |
| P   | 4818021 | 0.0642284 |
| G   | 4768358 | 0.0635663 |
| K   | 4476315 | 0.0596731 |
| V   | 4379135 | 0.0583776 |
| R   | 4251417 | 0.0566750 |
| T   | 4003422 | 0.0533691 |
| Q   | 3696682 | 0.0492799 |
| D   | 3613867 | 0.0481760 |
| I   | 3221094 | 0.0429400 |
| N   | 2767234 | 0.0368896 |
| F   | 2628834 | 0.0350446 |
| H   | 2017248 | 0.0268916 |
| Y   | 1937792 | 0.0258324 |
| C   | 1629506 | 0.0217227 |
| M   | 1625035 | 0.0216631 |
| W   |  867289 | 0.0115617 |

This function can yield the same results as the following code but in a
faster and more convenient manner.

``` bash
time seqkit.exe seq -g -j 12 -s -m 100 -M 2000 'GCF_000001405.40_GRCh38.p14_protein.faa'|perl -lne 'foreach(split //){$count{$_}++ if /\w/;} END {print "$_:$count{$_}" for sort {$count{$b} <=> $count{$a}} keys %count;}'
```

    ## L:7307961
    ## S:6452376
    ## E:5457680
    ## A:5094653
    ## P:4818021
    ## G:4768358
    ## K:4476315
    ## V:4379135
    ## R:4251417
    ## T:4003422
    ## Q:3696682
    ## D:3613867
    ## I:3221094
    ## N:2767234
    ## F:2628834
    ## H:2017248
    ## Y:1937792
    ## C:1629506
    ## M:1625035
    ## W:867289
    ## U:79
    ## X:26
    ## 
    ## real 0m19.564s
    ## user 0m18.843s
    ## sys  0m0.062s

Through comparative analysis, we observed that the differences in
results are not substantial when removing proteins with lengths greater
than 2000, greater than 3000, less than 100 or greater than 2000, and
less than 100 or greater than 3000 (see the table below).

``` r
vecSeqMin <- c(100, 100, 0, 0)
vecSeqMax <- c(3000, 2000, 2000, 3000)
vecDFNames <- paste0("min_", vecSeqMin, "-max_", vecSeqMax)
dfAAsFreq <- purrr::pmap(list(vecSeqMin, vecSeqMax), \(.x, .y) {
        alpha_count(r"(GCF_000001405.40_GRCh38.p14_protein.faa)",
            seq_min = .x, 
            seq_max = .y,
            verbose = F, simple = F
            )$Prop |> 
        d=>d*100 
}) |> as.data.frame() |> 
        `names<-`(vecDFNames)
rownames(dfAAsFreq) <- cppTools:::.vecAminoAcids
dfAAsFreq[order(-dfAAsFreq$`min_100-max_2000`), ] |> 
        knitr::kable(caption = "Table. ExAAs for Humans (GRCh38) Based on Various Sequence Length Cutoff Thresholds")
```

|     | min_100-max_3000 | min_100-max_2000 | min_0-max_2000 | min_0-max_3000 |
|:----|-----------------:|-----------------:|---------------:|---------------:|
| L   |         9.732057 |         9.742140 |       9.742923 |       9.732785 |
| S   |         8.689369 |         8.601572 |       8.598569 |       8.686471 |
| E   |         7.344946 |         7.275556 |       7.274885 |       7.344194 |
| A   |         6.800071 |         6.791610 |       6.792469 |       6.800829 |
| P   |         6.426248 |         6.422836 |       6.421316 |       6.424867 |
| G   |         6.315995 |         6.356631 |       6.356604 |       6.316055 |
| K   |         5.981210 |         5.967313 |       5.969600 |       5.983248 |
| V   |         5.813118 |         5.837763 |       5.837829 |       5.813230 |
| R   |         5.644409 |         5.667504 |       5.668263 |       5.645144 |
| T   |         5.346559 |         5.336906 |       5.335804 |       5.345543 |
| Q   |         4.987470 |         4.927995 |       4.927423 |       4.986828 |
| D   |         4.834985 |         4.817595 |       4.816295 |       4.833773 |
| I   |         4.281076 |         4.293995 |       4.293780 |       4.280908 |
| N   |         3.697020 |         3.688961 |       3.687950 |       3.696090 |
| F   |         3.461021 |         3.504462 |       3.505401 |       3.461961 |
| H   |         2.669183 |         2.689165 |       2.687614 |       2.667823 |
| Y   |         2.548895 |         2.583243 |       2.583435 |       2.549141 |
| C   |         2.134785 |         2.172271 |       2.174357 |       2.136749 |
| M   |         2.156276 |         2.166311 |       2.168865 |       2.158605 |
| W   |         1.135307 |         1.156171 |       1.156619 |       1.135756 |

Table. ExAAs for Humans (GRCh38) Based on Various Sequence Length Cutoff
Thresholds

``` r
suppressPackageStartupMessages(
        {
                library(ComplexHeatmap)
        }
) |> 
        suppressWarnings()
dfAAsFreq |> 
        as.matrix() |> t() |> 
        Heatmap3D(
                #rect_gp = gpar(col = "white", lwd = 2),
                col = circlize::colorRamp2(c(0, 5, 10), c("darkgreen", "orange", "darkred")),
                name = "Prop (%)", column_names_rot = 0
                )
```

![](README_files/figure-gfm/unnamed-chunk-5-1.pdf)<!-- -->
