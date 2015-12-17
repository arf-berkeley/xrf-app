library(shiny)
library(DT)
library(dplyr)
library(shinyIncubator)



shinyUI(navbarPage("XRF", id="nav",
tabPanel("Spectrum",
div(class="outer",
headerPanel("X-Ray Fluorescence Spectrum Viewer"),
sidebarLayout(
sidebarPanel(



actionButton("actionprocess", label = "Process Data"),
actionButton("actionplot", label = "Plot Spectrum"),
downloadButton('downloadPlot', "Plot"),


tags$hr(),

fileInput('file1', 'Choose Spectra', multiple=TRUE,
accept=c('text/csv',
'text/comma-separated-values,text/plain',
'.csv')),

tags$hr(),

element <- selectInput(
"element", "Element:",
c("(Ne) Neon" = "Ne.table",
"(Na) Sodium" = "Na.table",
"(Mg) Magnesium" = "Mg.table",
"(Al) Aluminum" = "Al.table",
"(Si) Silicon" = "Si.table",
"(P)  Phosphorous" = "P.table",
"(S)  Sulfur" = "S.table",
"(Cl) Chlorine" = "Cl.table",
"(Ar) Argon" = "Ar.table",
"(K)  Potassium" = "K.table",
"(Ca) Calcium" = "Ca.table",
"(Sc) Scandium" = "Sc.table",
"(Ti) Titanium" = "Ti.table",
"(V)  Vanadium" = "V.table",
"(Cr) Chromium" = "Cr.table",
"(Mn) Manganese" = "Mn.table",
"(Fe) Iron" = "Fe.table",
"(Co) Cobalt" = "Co.table",
"(Ni) Nickel" = "Ni.table",
"(Cu) Copper" = "Cu.table",
"(Zn) Zinc"= "Zn.table",
"(Ga) Gallium" = "Ga.table",
"(Ge) Germanium" = "Ge.table",
"(As) Arsenic" = "As.table",
"(Se) Selenium" = "Se.table",
"(Br) Bromium" = "Br.table",
"(Kr) Krypton" = "Kr.table",
"(Rb) Rubidium" = "Rb.table",
"(Sr) Strontium" = "Sr.table",
"(Y)  Yttrium" = "Y.table",
"(Zr) Zirconium" = "Zr.table",
"(Nb) Niobium" = "Nb.table",
"(Mo) Molybdenum" = "Mo.table",
"(Tc) Technicium" = "Tc.table",
"(Ru) Ruthenium" = "Ru.table",
"(Rh) Rhodium" = "Rh.table",
"(Pd) Paladium" = "Pd.table",
"(Ag) Silver" = "Ag.table",
"(Cd) Cadmium" = "Cd.table",
"(In) Indium" = "In.table",
"(Sn) Tin" = "Sn.table",
"(Sb) Antimony" = "Sb.table",
"(Te) Tellerium" = "Te.table",
"(I) Iodine" = "I.table",
"(Xe) Xenon" = "Xe.table",
"(Cs) Cesium" = "Cs.table",
"(Bs) Barium" = "Ba.table",
"(Ce) Cerium" = "Ce.table",
"(Pr) Praeseodymeum" = "Pr.table",
"(Nd) Neodymeum" = "Nd.table",
"(Pr) Promethium" = "Pr.table",
"(Sm) Samarium" = "Sm.table",
"(Eu) Europium" = "Eu.table",
"(Gd) Gadolinium" = "Gd.table",
"(Tb) Terbium" = "Tb.table",
"(Dy) Dysprosium" = "Dy.table",
"(Ho) Holmium" = "Ho.table",
"(Er) Erbium" = "Er.table",
"(Tm) Thullium" = "Tm.table",
"(Yb) Ytterbium" = "Yb.table",
"(Lu) Lutetium" = "Lu.table",
"(Hf) Halfnium" = "Hf.table",
"(Ta) Tantalum" = "Ta.table",
"(W)  Tungsten" = "W.table",
"(Re) Rhenium" = "Re.table",
"(Os) Osmium" = "Os.table",
"(Ir) Irridium" = "Ir.table",
"(Pt) Platinum" = "Pt.table",
"(Au) Gold" = "Au.table",
"(Hg) Mercury" = "Hg.table",
"(Tl) Thallium" = "Tl.table",
"(Pb) Lead" = "Pb.table",
"(Bi) Bismuth" = "Bi.table",
"(Po) Polonium" = "Po.table",
"(At) Astatine" = "At.table",
"(Rn) Radon" = "Rn.table",
"(Fr) Francium" = "Fr.table",
"(Ra) Radium" = "Ra.table",
"(Ac) Actinum" = "Ac.table",
"(Th) Thorium" = "Th.table",
"(Pa) Proactinum" = "Pa.table",
"(U)  Uranium" = "U.table"),
selected="Fe.table")




),



mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("distPlot", height = 455,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))))),
))
),

tabPanel("Counts",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(

downloadButton('downloadData', "Table"),

tags$hr(),

conditionalPanel(
'input.dataset === spectra.line.table',
checkboxGroupInput('show_vars', 'Elemental lines to show:',
names(spectra.line.table), selected = standard)
)),




mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Spectral Lines', dataTableOutput('mytable1')))

)
)

)



)),

tabPanel("PCA",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(
numericInput("knum", label = "K-Means", value=3),

sliderInput("spotsize", label = "Point Size", value=3, min=1, max=15),

checkboxInput('elipseplot1', "Elipse"),

tags$hr(),


downloadButton('downloadPlot2', "Plot")
),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Selected XRF Lines', plotOutput('xrfpcaplot',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))


))

))

)),

tabPanel("Timeseries",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

p("Create Plot"),
actionButton('timeseriesact1', "1"),
actionButton('timeseriesact2', "2"),
actionButton('timeseriesact3', "3"),
actionButton('timeseriesact4', "4"),
actionButton('timeseriesact5', "5"),

tags$hr(),

downloadButton('downloadPlot3a', "1"),
downloadButton('downloadPlot3b', "2"),
downloadButton('downloadPlot3c', "3"),
downloadButton('downloadPlot3d', "4"),
downloadButton('downloadPlot3e', "5"),


tags$hr(),

selectInput("elementtrend", "Element:", names(spectra.line.table), selected="Fe.K.alpha"),
selectInput("elementnorm", "Ratio:", names(spectra.line.table.norm), selected="None"),

selectInput(
"timecolour", "Time Series Type",
c(
"Black" = "Black",
"Smooth" = "Smooth",
"Ramp" = "Selected",
"Cluster" = "Cluster",
"Area" = "Area")
),

numericInput("intervalmm", label = "Interval Between Spectra (mm)", value=3),

tags$hr(),


sliderInput("smoothing", label = "Smoothed Mean Average", value=1, min=1, max=50),

sliderInput("linesize", label = "Line Size", value=1, min=1, max=15)


),

mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Time Series 1', plotOutput('timeseriesplot1',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))),

tabPanel('Time Series 2', plotOutput('timeseriesplot2',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))),

tabPanel('Time Series 3', plotOutput('timeseriesplot3',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))),

tabPanel('Time Series 4', plotOutput('timeseriesplot4',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))),

tabPanel('Time Series 5', plotOutput('timeseriesplot5',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))



))

))

)),

tabPanel("Ternary Diagram",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(


selectInput(
"ternarycolour", "Ternary Plot Type",
c(
"Black" = "Black",
"Cluster" = "Cluster"
)),

tags$hr(),


selectInput("axisa", "Axis A", names(spectra.line.table), selected="Al.K.alpha"),
selectInput("axisb", "Axis B", names(spectra.line.table), selected="Si.K.alpha"),
selectInput("axisc", "Axis C", names(spectra.line.table), selected="Ca.K.alpha"),
checkboxInput('terndensityplot', "Density Contour"),

tags$hr(),


downloadButton('downloadPlot5', "Plot")

),

mainPanel(
tabPanel('Ternary Plot', plotOutput('ternaryplot',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))))


))

)),

tabPanel("Elemental Ratios",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

selectInput(
"ratiocolour", "Ratio Plot Type",
c(
"Black" = "Black",
"Cluster" = "Cluster"
)),

tags$hr(),


selectInput("elementratioa", "Element A", names(spectra.line.table), selected="Fe.K.alpha"),
selectInput("elementratiob", "Element B", names(spectra.line.table), selected="Ca.K.alpha"),

selectInput("elementratioc", "Element C", names(spectra.line.table), selected="Ti.K.alpha"),
selectInput("elementratiod", "Element D", names(spectra.line.table), selected="K.K.alpha"),

tags$hr(),

checkboxInput('elipseplot2', "Elipse"),

sliderInput("spotsize2", label = "Point Size", value=3, min=1, max=15),

tags$hr(),


downloadButton('downloadPlot4', "Plot")



),

mainPanel(
tabPanel('Element Ratios', plotOutput('elementratiotimeseries',
dblclick = "plot1_dblclick", height = 500, width= 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))

))

))


)


))










