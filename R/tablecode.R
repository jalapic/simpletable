#' Returns a Simple Table in RMarkdown HTML output
#'
#' @param df The dataframe to turn into a table
#' @param tabletype the name of the table style.  The options currently are
#' \code{minimal}, \code{onecol}, \code{box}, \code{hoverTable}, \code{zebra},
#' \code{zebra1}, \code{another}, \code{simple}, \code{gridtable}
#' @param width the width of the table
#' @return A table of the desired style
#' @examples
#' tabletype(paintings, tabletype="minimal")
#' @export


tablecode <- function(df, tabletype="tabletype", width = "80%"){
  tmp <- noquote(apply(df, 1, function(x) paste0("<td data-th='", sprintf("%s",colnames(df)), "'>", x, "</td>", collapse=" ")))


if(tabletype=="minimal"){


  cat(
    c(
      noquote("<style>
        table#minimal {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; border-collapse: collapse; font-family: 'Lucida Sans Unicode','Lucida Grande',Sans-Serif; text-align: left; min-width: 480px; margin-bottom:25px; }
      table#minimal th { border-bottom: 2px solid #444; color: #666666; font-size: 18px; font-weight: bold; padding: 10px; }
      table#minimal td { border-bottom: 1px solid #CCCCCC; color: #999; padding: 8px 10px; }
      table#minimal tbody tr:hover td {color:#444;}
      </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
    )
    )
}

###


if(tabletype=="box"){


  cat(
    c(
      noquote("<style>
      table#box {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px;min-width:480px;text-align:left;border-collapse:collapse;margin-bottom:25px; }
table#box th {font-size:18px;font-weight:bold;background:#ccc;border-top:4px solid #ddd;border-bottom:1px solid #fff;color:#666666;padding:10px;}
table#box td {background:#f9f9f9;border-bottom:1px solid #fff;color:#999;border-top:1px solid transparent;padding:8px 10px;}
table#box tr:hover td {background:#FFCCFF;color:#444;}
</style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
    )
  )
}

###

if(tabletype=="onecol"){


  cat(
    c(
      noquote("<style>
table#onecol {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; min-width:480px; text-align:left; border-collapse:collapse; margin-bottom:25px;}
table#onecol th {font-size:18px; font-weight:normal; color:#FF6666; padding:12px 15px;}
table#onecol td {color:#999; border-top:1px solid #ccc; padding:10px 15px;}
table#onecol {background:#F0F0F0 ; border-right:10px solid transparent; border-left:10px solid transparent;}
table#onecol tr:hover td {color:#400000; background:#FFFFFF;}
</style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
    )
  )
}

###

if(tabletype=="simple"){


  cat(
    c(
      noquote("<style>
table#simple {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; border-top:1px solid #CFCFCF; border-left:1px solid #CFCFCF; border-right:0; border-bottom:0; width:100%;}
table#simple td, .simple-style th {border-right:1px solid #CFCFCF; border-bottom:1px solid #CFCFCF; text-align:center; padding:5px 0; width:20%;}
table#simple th {background-color:#dedede; font-size:120%;text-shadow: 0 1px 0 #fff; text-align:center}
table#simple tr:nth-child(even) {background: #fff;}
table#simple tr:nth-child(odd) {background: #F6F6F6;}
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
}

###

if(tabletype=="gridtable"){


  cat(
    c(
      noquote("<style>
table#gridtable {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; color:#333333; border-width: 1px; border-color: #666666; border-collapse:collapse;}
table#gridtable th {font-size:18px;border-width: 1px; padding: 8px; border-style: solid;border-color: #666666; background-color: #dedede;}
table#gridtable td {border-width: 1px; padding: 8px; border-style: solid; border-color: #666666; background-color: #ffffff;}
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
    )
  )
}


###

if(tabletype=="hoverTable"){


  cat(
    c(
      noquote("<style>
table#hoverTable {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; width:100%; border-collapse:collapse; }
table#hoverTable td{ padding:7px; border:#4e95f4 2px solid;  }
table#hoverTable th {font-size:18px;padding:7px; border:#4e95f4 2px solid;  }
table#hoverTable tr{background: #b8d1f3;}
table#hoverTable tr:hover {background-color: #ffff99;  }
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
}



###

if(tabletype=="flattable3"){


  cat(
    c(
      noquote("<style>
table#flattable3 {margin-bottom: 20px;border-collapse:collapse;  font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px;
border: none; border-radius: 3px; -webkit-border-radius: 3px; -moz-border-radius: 3px; background: #52be7f;}
table#flattable3 tr:hover {  	background: rgba(0,0,0,0.1);}
table#flattable3 td {box-shadow: inset 0 -1px rgba(0,0,0,0.25), inset 0 1px rgba(0,0,0,0.25);}
table#flattable3 th {font-weight: normal;-webkit-font-smoothing: antialiased;padding: 1em;color: rgba(0,0,0,0.45);
text-shadow: 0 0 1px rgba(0,0,0,0.1);font-size: 1.1em;  }
table#flattable3 td {color: #f7f7f7;padding: 0.7em 1em 0.7em 1.15em;text-shadow: 0 0 1px rgba(255,255,255,0.1);font-size: 1.1em;}
table#flattable3 tr {-webkit-transition: background 0.3s, box-shadow 0.3s;-moz-transition: background 0.3s, box-shadow 0.3s;
		transition: background 0.3s, box-shadow 0.3s;}
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
}


###

if(tabletype=="another"){


  cat(
    c(
      noquote("<style>
table#another { border-collapse: collapse; border: 1px solid #839E99;
background: #f1f8ee; font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; color: #033; }
table#another td, th { padding: 3px 3px .75em 3px; line-height: 1.3em; }
table#another th {font-size:18px; background: #839E99; color: #fff; font-weight: normal; text-align: left; padding-right: .1em; vertical-align: top; }
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
  }

###

if(tabletype=="zebra"){


  cat(
    c(
      noquote("<style>
table#zebra {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; text-align:center; margin-bottom:25px; background-color: #111111;}
table#zebra th {color: #444; font-size: 18px; font-weight: bold; padding: 10px 8px; background-color: #FF9999;}
table#zebra td {color: #777;padding: 8px;}
table#zebra tr:nth-child(even) { color: black; background-color: #eee;}
table#zebra tr:nth-child(odd) {color: black; background-color: #fff;}
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
}


###

if(tabletype=="zebra1"){


  cat(
    c(
      noquote("<style>
table#zebra1 {font-family: 'Helvetica', 'Arial', sans-serif; font-size:16px; text-align:left; margin-bottom:25px; background-color: #FF9999;}
table#zebra1 th {color: #444; font-size: 18px; font-weight: bold; padding: 10px 8px;}
table#zebra1 td {color: #777;padding: 8px;}
table#zebra1 tr:nth-child(even) { color: black; background-color: #eee;}
table#zebra1 tr:nth-child(odd) {color: black; background-color: #fff;}
              </style>"),

      noquote(paste0("<table id='", tabletype,"' style='width:", width, "'> ")),
      noquote(paste0("<tr>", paste0("<th>", sprintf("%s",colnames(df)), "</th>", collapse=" "), "</tr>")),
      noquote(unlist(lapply(tmp, function(x) paste0("<tr>", x, "</tr>")))),
      noquote("</table>")
      )
    )
}


#end
}
