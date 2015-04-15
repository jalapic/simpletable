#' Defines the css style for a table
#'
#' @param name The unique id for the style to be referenced by \code{tablecode()}
#' @param colh The color of the header
#' @param colh.text The color of the text in the header
#' @param col The background color
#' @param col.text The color of text
#' @param border.style The style of border. Options
#'   are \code{none}, \code{hidden},
#'    \code{solid}, \code{dotted}, \code{double}, \code{dashed},
#'    \code{groove}, \code{ridge}, \code{inset}, \code{outset}
#' @param border.col Border color
#' @param border.wt Border weight - needs to be an integer
#' @param font.size a vector of font sizes for i) header ii) cells
#' @param lineheight The height of rows
#' @param hfont.wt The weight of the font in the header
#'   options are \code{normal}, \code{bold}, \code{bolder}, \code{lighter},
#'   or can be a number (usually between 100-900, default is 200).
#' @param htext.align The alignment of the header text
#'   are \code{left}, \code{center},
#'    \code{right}, \code{justify}
#' @param text.align The alignment of the cell text
#'   are \code{left}, \code{center},
#'    \code{right}, \code{justify}
#' @param borderh.style The style of border at bottom of header. Options
#'   are same as for \code{border.style}
#' @param borderh.col Border at bottom of header color
#' @param borderh.wt Border at bottom of header weight - needs to be an
#'    integer
#' @param borderb.style The style of border at the bottom of cells. Options
#'   are same as for \code{border.style}
#' @param borderb.col Border color at bottom of rows
#' @param borderb.wt Border weight at bottom of rows
#' @param pad The padding around borders of cells.
#' @param padh The padding around borders of header.
#' @param collapse The border collapse style - can be
#'    \code{collapse},  \code{separate},  \code{initial}
#' @return A css style
#' @examples
#' stylecode(name="uniqueid")
#' tablecode(paintings, tabletype="uniqueid", width="80%")
#' @export


stylecode<-function(name, colh="#B8B8B8", colh.text="#111111", col="#F0F0F0", col.text="#111111",
                    border.col="gray55", border.wt=1, border.style="solid", font.size=c(18,16), lineheight=1.3,
                    hfont.wt=200, borderh.wt=2, borderh.col="#CC0000", borderh.style="none", text.align="left",
                    htext.align="left", borderb.wt=1, borderb.col="#CCCCCC", borderb.style="none",
                    pad=c(3,3,12,3), padh=c(3,3,12,3), collapse="collapse")
  {

cat(
  noquote(
    paste(
      "<style>",
      paste0("table#",name, collapse="")
      ,"{border-collapse: ",
      collapse,
      ";",
      "border-style: ", paste0(border.style, ";", collapse=""),
      "border-width: ", paste0(border.wt, ";", collapse=""),
      "border-color: ", paste0(border.col, ";", collapse=""),
      " font-family: 'Helvetica', 'Arial', sans-serif; font-size:",
      paste0(font.size[2], "px;", collapse=""),
      " }",


      paste0("table#",name, collapse=""),
      "td { padding: ",
      paste0(pad[1], "px ", pad[2], "px ", pad[3], "px ", pad[4], "px ", collapse=""),


      "; line-height: ",
      paste0(lineheight, "em;", collapse=""),


      "background: ",
      paste0(col, "; ", collapse=""),
      "color:",
      paste0(col.text, "; ", collapse=""),
      "text-align: ",
      paste0(text.align, "; ", collapse=""),


      "border-bottom: ",
      paste0(borderb.style, " ", collapse=""),
      paste0(borderb.wt, "px ", collapse=""),
      paste0(borderb.col, " ;", collapse=""),


      " }",


      paste0("table#",name, collapse="")
      ,"th {text-shadow: none; font-size:",
      paste0(font.size[1], "px", collapse=""),
      "; background:",
      paste0(colh, "; ", collapse=""),
      " color: ",
      paste0(colh.text, "; ", collapse=""),
      " font-weight: ",
      paste0(hfont.wt, "; ", collapse=""),
      "text-align: ",
      paste0(htext.align, "; ", collapse=""),
      "; padding-right: .1em;  padding-left: .2em; vertical-align: middle;",

      " padding: ",
      paste0(padh[1], "px ", padh[2], "px ", padh[3], "px ", padh[4], "px ", collapse=""),

       "; line-height: ",
      paste0(lineheight, "em;", collapse=""),

      "border-bottom: ",
      paste0(borderh.style, " ", collapse=""),
      paste0(borderh.wt, "px ", collapse=""),
      paste0(borderh.col, " ;", collapse=""),

      "}",
     "</style>"
    )
  )
)
}
