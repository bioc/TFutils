
#' gadget to help sort through tags naming TFs
#' @importFrom shiny selectInput dataTableOutput reactive renderDataTable observeEvent
#' @importFrom shiny runGadget stopApp
#' @import miniUI
#' @param gscoll a GSEABase GeneSetCollection
#' @param initTF character(1) initial TF string for app
#' @param gadtitle character(1) a title for the gadget panel
#' @note Will use TFutils::gwascat_hg19_chr17 to look for 'MAPPED_GENE' field entries matching targets, also hardcoded to use org.Hs.eg.db to map symbols
#' @return on app conclusion a data.frame is returned
#' @examples
#' if (interactive()) TFtargs()
#' @export
TFtargs = function(gscoll=TFutils::tftColl, initTF="VDR_Q3",
   gadtitle="Search for a TF; its targets will be checked for mapped status in GWAS catalog") {
  ui <- miniPage(gadgetTitleBar(gadtitle), 
                 miniContentPanel(
                    selectInput("tfsel", "TF:", names(gscoll),
                         selected = initTF),
                    dataTableOutput("tab"))
                ) # end page
  server <- function(input, output, session) {
    getTab = reactive({
      grabTab(input$tfsel, TFutils::tftColl, org.Hs.eg.db::org.Hs.eg.db, TFutils::gwascat_hg19_chr17)
      })
    output$tab <- renderDataTable({
      getTab()
    })
    observeEvent(input$done, {
      stopApp(getTab())
    })
  }
  runGadget(ui, server)
}
