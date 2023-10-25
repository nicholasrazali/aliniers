library(shiny)
#devtools::install_github('nicholasrazali/package')
library(AljabarLinear)
library(shinyMatrix)
library(plotly)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(shinythemes)


hide_hitung <- function(session){
  updateNumericInput(session, "hit_rows_1", value = 2)
  updateNumericInput(session, "hit_cols_1", value = 2)
  hide("hasil_perhitungan")
  hide("hitung_perh")
  hide("hit_matriks_1")
  hide("hit_matriks_2")
  hide("hit_input_matrix_1")
  hide("hit_input_matrix_2")
  hide("hit_reset_btn")
  hide("hit_submit_1")
  hide("hit_submit_2")
  hide("hit_radio")
  hide("hitung")
  hide("hit_output_matrix_1")
  hide("hit_output_matrix_2")
  hide("hit_matrix_input_1")
  hide("hit_matrix_input_2")
  hide("output_matrix_hit_2")
  removeClass(selector = "#step6_perhitungan", class = "active")
  removeClass(selector = "#step5_perhitungan", class = "active")
  removeClass(selector = "#step4_perhitungan", class = "active")
  removeClass(selector = "#step3_perhitungan", class = "active")
  removeClass(selector = "#step2_perhitungan", class = "active")
  addClass(selector = "#step1_perhitungan", class = "active")
  show("submit_perhitungan")
}
hide_persamaan <- function(session){
  updateNumericInput(session, "pers_ukuran", value = 2)
  hide("hasil_persamaan")
  hide("pers_matriks")
  hide("pers_input_matrix")
  hide("pers_reset_btn")
  hide("pers_submit_btn")
  hide("pers_radio")
  hide("persamaan")
  hide("pers_output_matrix")
  hide("pers_matrix_input")
  hide("pers_vec")
  hide("pers_output_vector")
  hide("output_pers_vec")
  hide("gauss_manual")
  removeClass(selector = "#step5_persamaan", class = "active")
  removeClass(selector = "#step4_persamaan", class = "active")
  removeClass(selector = "#step3_persamaan", class = "active")
  removeClass(selector = "#step2_persamaan", class = "active")
  addClass(selector = "#step1_persamaan", class = "active")
  show("submit_persamaan")
}
hide_determinan <- function(session){
  updateNumericInput(session, "det_ukuran", value = 2)
  hide("hasil_determinan")
  hide("det_matriks")
  hide("det_input_matrix")
  hide("det_reset_btn")
  hide("det_submit_matrix")
  hide("det_radio")
  hide("determinan")
  hide("det_output_matrix")
  hide("det_matrix_input")
  removeClass(selector = "#step4", class = "active")
  removeClass(selector = "#step3", class = "active")
  removeClass(selector = "#step2", class = "active")
  addClass(selector = "#step1", class = "active")
  show("submit_determinan")
}
hide_invers <- function(session){
  updateNumericInput(session, "inv_ukuran", value = 2)
  hide("hasil_invers")
  hide("inv_matriks")
  hide("inv_input_matrix")
  hide("inv_reset_btn")
  hide("inv_submit_matrix")
  hide("inv_radio")
  hide("invers")
  hide("inv_output_matrix")
  hide("inv_matrix_input")
  removeClass(selector = "#step4_invers", class = "active")
  removeClass(selector = "#step3_invers", class = "active")
  removeClass(selector = "#step2_invers", class = "active")
  addClass(selector = "#step1_invers", class = "active")
  show("submit_invers")
}
hide_dotcross <- function(session){
  updateTextInput(session, "dot_vec", value = "")
  updateTextInput(session, "dot_vec_2", value = "")
  hide("hitung_crossdot")
  hide("plot_cross_ui")
  hide("dotcross")
  hide("plot_cross")
  hide("dotcross_radio")
  hide("dot_output_vector_2")
  hide("output_dot_2")
  hide("dot_output_vector_1")
  hide("dotcross_reset")
  removeClass(selector = "#step4_dotcross", class = "active")
  removeClass(selector = "#step3_dotcross", class = "active")
  removeClass(selector = "#step2_dotcross", class = "active")
  addClass(selector = "#step1_dotcross", class = "active")
  show("submit_dotcross")
}
hide_interpolasi <- function(session){
  updateTextInput(session, "x_vec", value = "")
  updateTextInput(session, "y_vec", value = "")
  hide("hasil_poli")
  hide("poli")
  hide("hitung_poli")
  hide("x_output_vector")
  hide("y_output_vector")
  hide("poli_reset")
  hide("output_koor_y")
  removeClass(selector = "#step4_polinomial", class = "active")
  removeClass(selector = "#step3_polinomial", class = "active")
  removeClass(selector = "#step2_polinomial", class = "active")
  addClass(selector = "#step1_polinomial", class = "active")
  show("submit_polinomial")
}
hide_hitvec <- function(session){
  updateTextInput(session, "hit_vec", value = "")
  updateTextInput(session, "hit_vec_2", value = "")
  hide("plot_hitung_ui")
  hide("hitung_hitvector")
  hide("hitvec")
  hide("plot_hitung")
  hide("hitvec_radio")
  hide("hitvec_output_vector_2")
  hide("output_hitvec_2")
  hide("hitvec_output_vector_1")
  hide("hitvec_reset")
  removeClass(selector = "#step4_hitvec", class = "active")
  removeClass(selector = "#step3_hitvec", class = "active")
  removeClass(selector = "#step2_hitvec", class = "active")
  addClass(selector = "#step1_hitvec", class = "active")
  show("submit_perhitungan_vector")
}
hide_proyeksi <- function(session){
  updateTextInput(session, "proyeksi_vec", value = "")
  updateTextInput(session, "proyeksi_vec_2", value = "")
  hide("proyeksi")
  hide("plot_proyeksi_ui")
  hide("hitung_proyeksi")
  hide("proyeksi_2")
  hide("plot_proyeksi")
  hide("proyeksi_radio")
  hide("proyeksi_output_vector_2")
  hide("output_proyeksi_2")
  hide("proyeksi_output_vector_1")
  hide("proyeksi_reset")
  removeClass(selector = "#step4_proyeksi", class = "active")
  removeClass(selector = "#step3_proyeksi", class = "active")
  removeClass(selector = "#step2_proyeksi", class = "active")
  addClass(selector = "#step1_proyeksi", class = "active")
  show("submit_proyeksi")
}
is_valid_fraction <- function(value) {
  fraction_parts <- strsplit(value, "/")[[1]]
  if (length(fraction_parts) != 2)
    return(FALSE)
  numerator <- as.integer(fraction_parts[1])
  denominator <- as.integer(fraction_parts[2])
  !any(is.na(numerator), is.na(denominator), denominator == 0)
}

#### ui ####
ui <- fluidPage(
  theme = shinytheme("united"),
  useShinyjs(),
  tags$head(
    tags$style(
      #### css ####
      HTML("
        .reset-button {
          text-align: right;
        }

        #full-screen {
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          margin: 0;
          padding: 0;
          overflow: auto;
        }

        .btn-default {
          background-color: #e9541f;
        }


        .splash-screen {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: #ffffff;
          z-index: 9999;
          display: flex;
          justify-content: center;
          align-items: center;
        }

        .navbar {
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }

       .navbar-brand {
          display: flex;
          align-items: center;
          padding-top: 0;
          padding-bottom: 0;
        }

        .navbar-brand img {
          height: 50px;
          margin-right: 10px;
        }

        .navbar-nav li:hover {
          background-color: red;
        }

        .navbar-default .navbar-nav>.active>a,
        .navbar-default .navbar-nav>.active>a:focus,
        .navbar-default .navbar-nav>.active>a:hover {
          font-weight: bold;
          font-size: 16px;
        }

        .step-indicator {
          display: flex;
          align-items: center;
          justify-content: space-between;
          border: 1px solid black;
          padding: 10px;
        }

        .step-line {
          flex: 1;
          height: 2px;
          background-color: black;
        }

        .step {
          background-color: transparent;
          color: #000000;
          padding: 10px;
          margin-right: 5px;
          margin-left : 5px;
          display: inline-block;
          cursor: pointer;
          border-style:solid;
        }

        .step.active {
          color: #ffffff;
          background-color: green;
        }

        .justify-text {
          text-align: justify;
        }

        a {
          cursor: pointer;
        }

      ")
    ),
    #### js ####
    tags$script(
      HTML("
        $(document).ready(function() {
          setTimeout(function() {
            $('#splash-screen').fadeOut(1000);
          }, 2000);
        });

        $(document).ready(function() {
        var favicon = document.createElement('link');
        favicon.type = 'image/png';
        favicon.rel = 'icon';
        favicon.href = 'https://drive.google.com/uc?export=view&id=1dr6JCc7Jp57WfsvmTqBH_FFb6lK_g-xN';
        document.getElementsByTagName('head')[0].appendChild(favicon);
      });
      ")
    )
  ),
  tags$div(
    id = "splash-screen",
    class = "splash-screen",
    img(src = "https://drive.google.com/uc?export=view&id=1dr6JCc7Jp57WfsvmTqBH_FFb6lK_g-xN",
        width = "400px", height = "300px")
  ),
  tags$div(id = "full-screen",
  navbarPage(
    id = "tes",
    title =div(
      style = "display: flex; align-items: center;",
      img(src = "https://drive.google.com/uc?export=view&id=1dr6JCc7Jp57WfsvmTqBH_FFb6lK_g-xN", height = 50),
      h3("ALINIERS", style = "margin-left: 10px;")),
    windowTitle = "Aliniers",
    header = NULL,
    tabPanel(
      #### home ####
      "Home",
      style = "margin-left : 10%;margin-right:10%;font-size:16px;",
      h3(tags$b("Tentang Aplikasi")),

      h4("Selamat datang di aplikasi Aliniers!"),
      h4(class = "justify-text","Aplikasi Aliniers adalah solusi lengkap untuk perhitungan aljabar linear yang memungkinkan untuk menjelajahi dan mempelajari berbagai topik matriks dan vektor. Nikmati fitur-fitur yang telah disediakan untuk mempermudah pemahaman pengguna mengenai aljabar linear."),

      div(
        style = "display:flex;align-items:center;",
        img(src = "https://drive.google.com/uc?export=view&id=1TE7nwTsTSySCinyMBDvuPlP121N10NJp",
            width = "300px", height = "200px"),

        h4(class = "justify-text","Aliniers hadir dengan algoritma canggih yang dapat membantu pengguna dengan cepat dan akurat menghitung determinan, invers, dan perhitungan matriks lainnya. Selain itu, jika pengguna tertarik dengan persamaan linear atau polinomial, Aliniers juga menawarkan solusi yang mudah dan efisien."),
      ),

      div(
        style = "display:flex;align-items:center;",
        h4(class = "justify-text","Aliniers juga memberikan pemahaman yang mendalam tentang vektor dan operasi yang terkait dengannya, sehingga dapat membantu dalam perhitungan dot product atau cross product, proyeksi vektor, jarak, atau transformasi linear. Aliniers memiliki semua yang dibutuhkan dalam mempelajari konsep-konsep ini dengan lebih mudah."),
        img(style = "margin-left:10px;",src = "https://drive.google.com/uc?export=view&id=16FHlyNYFx6cG6sv2l-ljGNaeO_Rbt0rD",
            width = "300px", height = "200px"),
      ),

      h4("Selamat belajar dan semoga aplikasi Aliniers dapat membantu memahami konsep-konsep dalam aljabar linear dengan lebih baik."),


      tags$br(),
      tags$br(),
      tags$br(),
      tags$footer(
        tags$b("Copyright ©2023 Nicholas")
      )
    ),
      #### topik ####
    tabPanel("Topik",
             style = "margin-left : 10%;margin-right:10%;font-size:16px;margin-bottom:100px;",
             h4(tags$b("Berikut adalah daftar topik yang dapat  dieksplorasi dalam materi matriks:")),
             tags$ol(
                 tags$li(h4(
                   tags$a(
                   "Determinan",
                   class = "nav-link",
                   "data-toggle" = "tab",
                   "data-value" = "Determinan",
                   onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_matriks', Math.random());
                              setTimeout(function() {
                                $('#nav_matrix li a[data-value=Determinan]').tab('show');
                              }, 600);"
                 ))),
                 h4("Pahami konsep untuk menemukan determinan matriks dengan metode row reduction atau kofaktor."),
                 tags$li(h4(
                   tags$a(
                     "Invers",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Invers",
                     onclick = HTML("$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_matriks', Math.random());
                              setTimeout(function() {
                                $('#nav_matrix li a[data-value=Invers]').tab('show');
                              }, 600);")
                   ))),
                 h4("Pelajari langkah-langkah untuk menemukan invers sebuah matriks dengan metode row reduction atau adjoint matriks."),
                 tags$li(
                   tags$a(h4(
                     "Perhitungan Matriks",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Perhitungan",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_matriks', Math.random());
                              setTimeout(function() {
                                $('#nav_matrix li a[data-value=Perhitungan]').tab('show');
                              }, 600);"
                   ))),
                 h4("Pelajari operasi dasar pada matriks seperti penjumlahan, pengurangan, dan perkalian."),
                 tags$li(
                   tags$a(h4(
                     "Persamaan Linear",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Persamaan Linear",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_matriks', Math.random());
                              setTimeout(function() {
                                $('#nav_matrix li a[data-value=\"Persamaan Linear\"]').tab('show');
                              }, 600);"
                   ))),
                 h4("Pelajari cara memecahkan sistem persamaan linear menggunakan eliminasi Gauss Jordan dengan sistem maupun input manual atau dengan Cramer's Rule."),
                 tags$li(h4(
                   tags$a(
                     "Polinomial Interpolasi",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Polinomial",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_matriks', Math.random());
                              setTimeout(function() {
                                $('#nav_matrix li a[data-value=Polinomial]').tab('show');
                              }, 600);"
                   ))),
                 h4("Pelajari cara menggunakan matriks dalam memperkirakan polinomial yang melalui titik-titik data yang diberikan."),
             ),
               tags$br(),

             h4(tags$b("Selain itu, dalam materi vektor, kami menyediakan topik-topik berikut:")),
               tags$ol(
                 tags$li(h4(
                   tags$a(
                     "Perhitungan Vektor",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Perhitungan",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Vektor', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_vektor', Math.random());
                              setTimeout(function() {
                                $('#nav_vector li a[data-value=Perhitungan]').tab('show');
                              }, 600);"
                   ))),
                 h4("Pelajari operasi dasar pada vektor seperti penjumlahan dan pengurangan."),
                 tags$li(h4(
                   tags$a(
                     "Proyeksi",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Proyeksi",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Vektor', {priority: 'event'});
                              Shiny.onInputChange('updateFunctions_vektor', Math.random());
                              setTimeout(function() {
                                $('#nav_vector li a[data-value=Proyeksi]').tab('show');
                              }, 600);"
                   ))),
                 h4("Pelajari cara menghitung proyeksi vektor pada vektor lainnya serta panjang vektor tersebut."),
                 tags$li(h4(
                   tags$a(
                     "Dot Product and Cross Product",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Dot and Cross",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                                Shiny.setInputValue('functions', 'Vektor', {priority: 'event'});
                                Shiny.onInputChange('updateFunctions_vektor', Math.random());
                                setTimeout(function() {
                                  $('#nav_vector li a[data-value=\"Dot and Cross\"]').tab('show');
                                }, 600);"
                  ))),
                 h4("Pelajari cara menghitung dot product dan cross product dalam perhitungan vektor."),

                 tags$li(h4(
                   tags$a(
                     "Distance",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Distance",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                                Shiny.setInputValue('functions', 'Vektor', {priority: 'event'});
                                Shiny.onInputChange('updateFunctions_vektor', Math.random());
                                setTimeout(function() {
                                  $('#nav_vector li a[data-value=Distance]').tab('show');
                                }, 600);"
                   ))),
                 h4("Pelajari cara menghitung jarak antara dua titik dalam ruang menggunakan vektor,  jarak antara titik dan garis dalam ruang dua dimensi, serta jarak antara titik dan bidang dalam ruang tiga dimensi."),
                 tags$li(h4(
                   tags$a(
                     "Transformasi Linear",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Transformasi Linear",
                     onclick = "$('#tes li:nth-child(3) a').tab('show');
                                Shiny.setInputValue('functions', 'Vektor', {priority: 'event'});
                                Shiny.onInputChange('updateFunctions_vektor', Math.random());
                                setTimeout(function() {
                                  $('#nav_vector li a[data-value=\"Transformasi Linear\"]').tab('show');
                                }, 600);"
                   ))),
                 h4("Pelajari cara melakukan transformasi linear dari vektor yang terletak pada dua dimensi ataupun tiga dimensi, seperti refleksi, rotasi, proyeksi, kontraksi, ataupun dilatasi.")
               )
        ),
    tabPanel(
      #### menu utama ####
      "Perhitungan",
      style = "margin-bottom:10%;",
      sidebarLayout(
        sidebarPanel(
          selectInput("functions", label = tags$span("Pilih Jenis Materi", style = "color: white;"), choices = c("Matriks", "Vektor"), selected = "Matriks"),
          width = 2,
          style = "background-color:#e9541f;"
        ),
        mainPanel(
          uiOutput("dynamicTabs"),
          width = 10
        )
      )
    ),
      #### bantuan ####
    tabPanel("Bantuan",
             style = "margin-left : 10%;margin-right:10%;font-size:16px;",
             h3(tags$b(tags$u("Tahapan Menggunakan Aplikasi"))),
             tags$br(),

             h4(tags$b("Melalui Tab Topik")),
             tags$ol(
               tags$li(
                 div(h4(
                   "Memilih Tab ",
                   tags$a(
                     "Topik",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Invers",
                     onclick = HTML("$('#tes li:nth-child(2) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              ")
                   )
                 )
               )),
               tags$li(h4("Memilih topik dari materi yang tersedia, dengan menekan nama dari topik")),
               tags$li(h4("Melakukan input data sesuai dengan yang diminta")),
               tags$li(h4("Memilih metode yang tersedia untuk untuk melakukan perhitungan")),
             ),
             tags$br(),
             tags$br(),

             h4(tags$b("Melalui Tab Perhitungan")),
             tags$ol(
               tags$li(h4(
                 div(
                   "Memilih Tab ",
                   tags$a(
                     "Perhitungan",
                     class = "nav-link",
                     "data-toggle" = "tab",
                     "data-value" = "Invers",
                     onclick = HTML("$('#tes li:nth-child(3) a').tab('show');
                              Shiny.setInputValue('functions', 'Matriks', {priority: 'event'});
                              ")
                   )
                 )
               )),
               tags$li(h4("Memilih jenis materi yang ingin digunakan")),
               tags$li(h4("Memilih topik dari materi yang tersedia")),
               tags$li(h4("Melakukan input data sesuai dengan yang diminta")),
               tags$li(h4("Memilih metode yang tersedia untuk untuk melakukan perhitungan")),
             )
             ),

  ),
  )
)


#### server ####
server <- function(input, output, session) {
  observeEvent(input$updateFunctions_matriks, {
    updateSelectInput(session, "functions", selected = "Matriks")
  })

  observeEvent(input$updateFunctions_vektor, {
    updateSelectInput(session, "functions", selected = "Vektor")
  })

  output$dynamicTabs <- renderUI(
  if(input$functions == "Matriks"){
      #### matrix ####
      navbarPage(
        id = "nav_matrix",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### determinan ######
        tabPanel("Determinan",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1", "Langkah 1", br(), "Memasukkan Ukuran Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2", "Langkah 2", br(), "Masukkan Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3", "Langkah 3", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("det_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("det_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 uiOutput("submit_determinan"),

                 uiOutput("det_matrix_input"),
                 uiOutput("det_error_2"),
                 uiOutput("det_submit_matrix"),
                 verbatimTextOutput("det_output_matrix"),

                 uiOutput("radio_det"),
                 uiOutput("hitung_determinan"),
                 uiOutput("hasil_determinan")),

        ##### invers ######
        tabPanel("Invers",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_invers", "Langkah 1", br(), "Memasukkan Ukuran Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_invers", "Langkah 2", br(), "Masukkan Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_invers", "Langkah 3", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_invers", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 id = "invers",
                 uiOutput("inv_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("inv_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 uiOutput("submit_invers"),

                 uiOutput("inv_matrix_input"),
                 uiOutput("inv_submit_matrix"),
                 verbatimTextOutput("inv_output_matrix"),

                 uiOutput("radio_inv"),
                 uiOutput("hitung_invers"),
                 uiOutput("hasil_invers")),

        ##### hitung ######
        tabPanel("Perhitungan",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_perhitungan", "Langkah 1", br(), "Memasukkan Ukuran Matriks Pertama"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_perhitungan", "Langkah 2", br(), "Masukkan Matriks pertama"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_perhitungan", "Langkah 3", br(), "Memasukkan Ukuran Matriks Kedua"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_perhitungan", "Langkah 4", br(), "Masukkan Matriks Kedua"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step5_perhitungan", "Langkah 5", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step6_perhitungan", "Langkah 6", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("hit_reset"),
                 h4("Matriks pertama"),
                 numericInput("hit_rows_1", "Masukkan Ukuran Baris:", value = 2, min = 1, max=5),
                 numericInput("hit_cols_1", "Masukkan Ukuran Kolom:", value = 2, min = 1, max=5),
                 uiOutput("submit_perhitungan"),

                 uiOutput("hit_matrix_input_1"),
                 uiOutput("hit_submit_matrix_1"),
                 verbatimTextOutput("hit_output_matrix_1"),

                 uiOutput("output_matrix_hit_2"),
                 verbatimTextOutput("hit_output_matrix_2"),


                 uiOutput("hit_matrix_input_2"),
                 uiOutput("hit_submit_matrix_2"),

                 uiOutput("radio_hit"),
                 uiOutput("hitung_perhitungan"),
                 uiOutput("hasil_perhitungan")),


        ##### persamaan linear ######
        tabPanel("Persamaan Linear",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_persamaan", "Langkah 1", br(), "Memasukkan Ukuran Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_persamaan", "Langkah 2", br(), "Masukkan Matriks"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_persamaan", "Langkah 3", br(), "Masukkan Vektor"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_persamaan", "Langkah 4", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step5_persamaan", "Langkah 5", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("pers_reset"),
                 h4("Ukuran matriks yang dimasukkan akan menghasilkan matriks n x n "),
                 numericInput("pers_ukuran", "Masukkan Ukuran Baris dan Kolom:", value = 2, min = 1, max = 5, width = "30%"),
                 uiOutput("submit_persamaan"),

                 uiOutput("pers_matrix_input"),
                 uiOutput("pers_submit_matrix"),
                 verbatimTextOutput("pers_output_matrix"),

                 uiOutput("output_pers_vec"),
                 verbatimTextOutput("pers_output_vector"),

                 uiOutput("radio_pers"),
                 uiOutput("hitung_persamaan"),

                 uiOutput("gauss_manual"),
                 uiOutput("hasil_persamaan")),


        ##### polinomial ######
        tabPanel("Polinomial",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_polinomial", "Langkah 1", br(), "Memasukkan Koordinat x"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_polinomial", "Langkah 2", br(), "Memasukkan Koordinat y"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_polinomial", "Langkah 3", br(), "Menekan Tombol Hitung"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_polinomial", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("poli_reset"),
                 h5(tags$b("Masukkan koordinat x (x1, x2, x3, ...., xn)")),
                 textInput("x_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 uiOutput("submit_polinomial"),
                 verbatimTextOutput("x_output_vector"),

                 uiOutput("output_koor_y"),
                 verbatimTextOutput("y_output_vector"),

                 uiOutput("hitung_poli"),
                 uiOutput("hasil_poli"))
      )

    } else if(input$functions == "Vektor"){
      #### vector ####
      navbarPage(
        id = "nav_vector",
        title = NULL,
        header = NULL,
        footer = NULL,
        ##### hitung vektor #####
        tabPanel("Perhitungan",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_hitvec", "Langkah 1", br(), "Memasukkan Vektor Pertama"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_hitvec", "Langkah 2", br(), "Memasukkan Vektor Kedua"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_hitvec", "Langkah 3", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_hitvec", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("hitvec_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("hit_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 uiOutput("submit_perhitungan_vector"),
                 verbatimTextOutput("hitvec_output_vector_1"),

                 uiOutput("output_hitvec_2"),
                 verbatimTextOutput("hitvec_output_vector_2"),

                 uiOutput("radio_hitvec"),
                 uiOutput("hitung_hitvec"),

                 uiOutput("plot_hitung_ui")),

        ##### proyeksi #####
        tabPanel("Proyeksi",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_proyeksi", "Langkah 1", br(), "Memasukkan Vektor Pertama"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_proyeksi", "Langkah 2", br(), "Memasukkan Vektor Kedua"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_proyeksi", "Langkah 3", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_proyeksi", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("proyeksi_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("proyeksi_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 uiOutput("submit_proyeksi"),
                 verbatimTextOutput("proyeksi_output_vector_1"),

                 uiOutput("output_proyeksi_2"),
                 verbatimTextOutput("proyeksi_output_vector_2"),

                 uiOutput("radio_proyeksi"),
                 uiOutput("hitung_proyeksi"),

                 uiOutput("plot_proyeksi_ui")),

        ##### dot cross ######
        tabPanel("Dot and Cross",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_dotcross", "Langkah 1", br(), "Memasukkan Vektor Pertama"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_dotcross", "Langkah 2", br(), "Memasukkan Vektor Kedua"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_dotcross", "Langkah 3", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_dotcross", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("dotcross_reset"),
                 h5(tags$b("Masukkan vektor pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("dot_vec",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 uiOutput("submit_dotcross"),
                 verbatimTextOutput("dot_output_vector_1"),

                 uiOutput("output_dot_2"),
                 verbatimTextOutput("dot_output_vector_2"),

                 uiOutput("radio_dotcross"),
                 uiOutput("hitung_dotcross"),

                 uiOutput("plot_cross_ui")),

        ##### distance ######
        tabPanel("Distance",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_distance", "Langkah 1", br(), "Memilih Perhitungan Jarak"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_distance", "Langkah 2", br(), "Memasukkan Titik"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_distance", "Langkah 3", br(), "Memasukkan Titik/Garis/Bidang"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_distance", "Langkah 4", br(), "Menekan Tombol Hitung"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step5_distance", "Langkah 5", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("dist_reset"),
                 radioButtons("dist_radio",
                              label = "Pilih perhitungan jarak terhadap",
                              choices = c("Jarak 2 titik", "Jarak titik ke garis", "Jarak titik ke bidang"),
                              selected = "none",
                              inline = TRUE),
                 uiOutput("hitung_distances"),

                 uiOutput("input_vector_dist"),
                 verbatimTextOutput("titik_output_vector_1"),
                 verbatimTextOutput("bidang_output_vector_1"),
                 verbatimTextOutput("titik_output_vector"),

                 uiOutput("input_vector_dist_2"),
                 verbatimTextOutput("titik_output_vector_2"),
                 verbatimTextOutput("bidang_output_vector_2"),
                 verbatimTextOutput("bidang_output_vector"),

                 uiOutput("hitung_distance"),

                 uiOutput("plot_distance_ui")),

        ##### transformasi ######
        tabPanel("Transformasi Linear",
                 tags$div(
                   class = "step-indicator",
                   tags$div(style = "text-align: center;",class = "step active", id = "step1_transformasi", "Langkah 1", br(), "Memasukkan Vektor"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step2_transformasi", "Langkah 2", br(), "Memilih Metode"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step3_transformasi", "Langkah 3", br(), "Memasukkan Parameter"),
                   tags$div(class = "step-line"),
                   tags$div(style = "text-align: center;",class = "step", id = "step4_transformasi", "Langkah 4", br(), "Perhitungan Selesai")
                 ),
                 tags$br(),
                 uiOutput("trans_reset"),
                 h5(tags$b("Masukkan vektor ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
                 textInput("trans_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
                 uiOutput("submit_transformasi"),
                 verbatimTextOutput("trans_output_vector"),

                 uiOutput("output_trans"),

                 uiOutput("radio_trans"),
                 uiOutput("hitung_transformasi"),
                 uiOutput("input_info_trans"),
                 verbatimTextOutput("refleksi_output_vector"),
                 verbatimTextOutput("proyeksi_output_vector"),
                 verbatimTextOutput("kontraksi_output_vector"),
                 verbatimTextOutput("rotasi_output_vector_alfa"),

                 uiOutput("rotasi_axis_input"),
                 verbatimTextOutput("rotasi_output_vector"),

                 uiOutput("plot_ui"))
      )


    }
  )

  #### input matrix invers ####

  output$submit_invers <- renderUI({
    list(
      tags$div(style="color:red;", "*ukuran maksimal matriks yaitu 5x5"),
      actionButton("inv_submit", "Next")
    )
  })

  matrix_data <- reactiveVal(NULL)
  observeEvent(input$inv_submit, {
    addClass(selector = "#step2_invers", class = "active")

    if(is.na(input$inv_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$inv_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$inv_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$inv_ukuran *40),"px")
    output$inv_matrix_input <- renderUI({
      list(
        h5(tags$b("Masukkan matriks",id = "id_matriks")),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "inv_input_matrix",
            value = matrix_data(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })

    hide("submit_invers")

    matrix_data(matrix("", nrow = input$inv_ukuran, ncol = input$inv_ukuran, dimnames = list(NULL, NULL)))

    show("inv_submit_matrix")
    output$inv_submit_matrix <-renderUI({
      list(
        div(style = "color:red","*input nilai matriks berupa angka"),
        actionButton("inv_submit_btn", "Next")
      )
    })

    show("inv_matrix_input")
    output$inv_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("inv_reset_btn", "Reset")
      )
    })
  })

  matrix_a <- reactiveVal(NULL)
  observeEvent(input$inv_submit_btn, {
    addClass(selector = "#step3_invers", class = "active")

    mat <- c()
    for (i in 1:(input$inv_ukuran * input$inv_ukuran)) {
      if(is.na(input$inv_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$inv_input_matrix[[i]])
    }
    matrix_a(matrix(mat, nrow = input$inv_ukuran, ncol = input$inv_ukuran))
    hide("inv_submit_matrix")
    output$inv_output_matrix <- renderPrint({
      matrix_a()
    })
    output$radio_inv <- renderUI(
      radioButtons("inv_radio",
                   label = "Pilih metode",
                   choices = c("Row Reduction", "Adjoint Matrix"),
                   selected = "none",
                   inline = TRUE))

    show("inv_output_matrix")
  })

  observeEvent(input$inv_radio, {
    removeClass(selector = "#step4_invers", class = "active")

    show("hitung_invers")
    hide("invers")
    hide("hasil_invers")
    output$hitung_invers <- renderUI(
      actionButton("hitung_inv","Hitung"),
    )
  })

  observeEvent(input$hitung_inv, {
    hide("hitung_inv")
    show("hasil_invers")

    output$hasil_invers <- renderUI(
      verbatimTextOutput("invers") %>% withSpinner()
    )

    show("invers")
    output$invers <- renderPrint(
      if(input$inv_radio == "Row Reduction"){
        invers_row_reduction(matrix_a())
      }
      else if(input$inv_radio == "Adjoint Matrix"){
        invers_adjoint_matriks(matrix_a())
      }
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 3000
    )

    isolate({
      Sys.sleep(3)
      addClass(selector = "#step4_invers", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

  })



  observeEvent(input$inv_reset_btn, {
    matrix_a(NULL)
    hide_invers(session)
  })


  #### input matrix determinan ####

  output$submit_determinan <- renderUI({
    list(
      tags$div(style="color:red;", "*ukuran maksimal matriks yaitu 5x5"),
      actionButton("det_submit", "Next")
    )
  })

  matrix_det <- reactiveVal(NULL)
  observeEvent(input$det_submit, {
    if(is.na(input$det_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$det_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$det_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    #### step indicator
    addClass(selector = "#step2", class = "active")

    width <- paste0((input$det_ukuran *40),"px")
    output$det_matrix_input <- renderUI({
      list(
        h5(tags$b("Masukkan matriks", id = "det_matriks")),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "det_input_matrix",
            value = matrix_det(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        )
      )
    })

    matrix_det(matrix("", nrow = input$det_ukuran, ncol = input$det_ukuran, dimnames = list(NULL, NULL)))
    output$det_submit_matrix <-renderUI({
      list(
        div(style = "color:red","*input nilai matriks berupa angka"),
        actionButton("det_submit_btn", "Next")
      )
    })
    show("det_submit_matrix")
    hide("submit_determinan")
    show("det_matrix_input")
    output$det_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("det_reset_btn", "Reset")
      )
    })
  })

  matrix_det_a <- reactiveVal(NULL)
  observeEvent(input$det_submit_btn, {
    mat <- c()
    for (i in 1:(input$det_ukuran * input$det_ukuran)) {
      if(is.na(input$det_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$det_input_matrix[[i]])
    }
    matrix_det_a(matrix(mat, nrow = input$det_ukuran, ncol = input$det_ukuran))
    hide("det_submit_matrix")

    addClass(selector = "#step3", class = "active")

    output$radio_det <- renderUI(
      radioButtons("det_radio",
                   label = "Pilih metode",
                   choices = c("Row Reduction", "Cofactor"),
                   selected = "none",
                   inline = TRUE))

    output$det_output_matrix <- renderPrint({
      matrix_det_a()
    })

    show("det_output_matrix")
  })

  observeEvent(input$det_radio, {
    removeClass(selector = "#step4", class = "active")

    show("hitung_det")
    hide("determinan")
    hide("hasil_determinan")
    output$hitung_determinan <- renderUI(
      actionButton("hitung_det","Hitung"),
    )
  })

  observeEvent(input$hitung_det, {
    hide("hitung_det")
    show("hasil_determinan")
    output$hasil_determinan <- renderUI(
      verbatimTextOutput("determinan") %>% withSpinner()
    )
    show("determinan")

    output$determinan <- renderPrint({
      if (input$det_radio == "Row Reduction") {
        determinan_row_reduction(matrix_det_a())
      } else if (input$det_radio == "Cofactor") {
        determinan_cofactor(matrix_det_a())
      }
    })

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 3000
    )

    isolate({
      Sys.sleep(3)
      addClass(selector = "#step4", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

  })


  observeEvent(input$det_reset_btn, {
    matrix_det_a(NULL)
    hide_determinan(session)
  })


  #### input matrix persamaan ####

  output$submit_persamaan <- renderUI({
    list(
      div(style="color:red;", "*ukuran maksimal matriks yaitu 5x5"),
      actionButton("pers_submit", "Next")
    )
  }
  )
  matrix_pers <- reactiveVal(NULL)
  observeEvent(input$pers_submit, {
    if(is.na(input$pers_ukuran))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$pers_ukuran > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$pers_ukuran < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    width <- paste0((input$pers_ukuran *40),"px")

    addClass(selector = "#step2_persamaan", class = "active")


    output$pers_matrix_input <- renderUI({
      list(
        h5(tags$b("Masukkan matriks", id = "pers_matriks_1")),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "pers_input_matrix",
            value = matrix_pers(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })
    matrix_pers(matrix("", nrow = input$pers_ukuran, ncol = input$pers_ukuran, dimnames = list(NULL, NULL)))

    show("pers_submit_matrix")
    output$pers_submit_matrix <-renderUI({
      list(
        div(style = "color:red","*input nilai matriks berupa angka"),
        actionButton("pers_submit_btn", "Next")
      )
    })

    hide("submit_persamaan")
    show("pers_matrix_input")

    output$pers_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("pers_reset_btn", "Reset")
      )
    })
  })

  matrix_pers_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_btn, {
    mat <- c()
    for (i in 1:(input$pers_ukuran * input$pers_ukuran)) {
      if(is.na(input$pers_input_matrix[[i]])) return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, as.numeric(input$pers_input_matrix[[i]]))
    }
    matrix_pers_a(matrix(mat, nrow = input$pers_ukuran, ncol = input$pers_ukuran))

    addClass(selector = "#step3_persamaan", class = "active")

    hide("pers_submit_matrix")

    output$pers_output_matrix <- renderPrint({
      matrix_pers_a()
    })

    show("pers_output_matrix")
    show("output_pers_vec")
    show("error_persamaan")
    output$output_pers_vec <- renderUI(list(
      h5(tags$b("Masukkan vektor hasil (pisahkan dengan koma)")),
      textInput("pers_vec", label = NULL, placeholder = "contoh : 1,2,3"),
      div(id="error_persamaan",style = "color:red","*input nilai vektor berupa angka sebanyak ukuran matriks"),
      actionButton("pers_submit_vec", "Next")
    ))
  })

  pers_vector_a <- reactiveVal(NULL)
  observeEvent(input$pers_submit_vec, {

    pers_vector_input <- as.numeric(strsplit(input$pers_vec, ",")[[1]])
    if(length(pers_vector_input) == 0)  return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))

    for(i in 1:length(pers_vector_input)){
      if(is.na(pers_vector_input[[i]]))  return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(pers_vector_input)!=input$pers_ukuran)  return(shinyalert("Kesalahan Input", "Banyaknya vektor harus sama dengan ukuran matriks", type = "error"))
    pers_vector_a(c(pers_vector_input))

    hide("pers_submit_vec")
    hide("error_persamaan")

    addClass(selector = "#step4_persamaan", class = "active")


    output$pers_output_vector <- renderPrint({
      pers_vector_a()
    })

    show("pers_output_vector")

    output$radio_pers <- renderUI(
      radioButtons("pers_radio",
                   label = "Pilih metode",
                   choices = c("Gauss Jordan", "Cramer's Rule","Input Manual"),
                   selected = "none",
                   inline = TRUE))

  })

  hasil_verbatim <- reactiveVal("")
  ket <- reactiveVal("")
  observeEvent(input$pers_radio, {
    removeClass(selector = "#step5_persamaan", class = "active")
    show("hitung_pers")
    hide("persamaan")
    hide("hasil_persamaan")

    output$hitung_persamaan <- renderUI(
      actionButton("hitung_pers", "Hitung")
    )

    hide("gauss_manual")
  })

  observeEvent(input$hitung_pers,{
    hide("hitung_pers")
    show("hasil_persamaan")
    output$hasil_persamaan <- renderUI(
      verbatimTextOutput("persamaan") %>% withSpinner()
    )
    show("persamaan")

    output$persamaan <- renderPrint(
      if(input$pers_radio == "Gauss Jordan"){
        gauss_jordan(matrix_pers_a(), pers_vector_a())
      }
      else if(input$pers_radio == "Cramer's Rule"){
        cramer_rule(matrix_pers_a(),pers_vector_a())
      }
    )

    if(input$pers_radio == "Input Manual"){
      addClass(selector = "#step5_persamaan", class = "active")
      show("gauss_manual")
      output$gauss_manual <- renderUI(
        list(
          sidebarLayout(
            sidebarPanel(
              radioButtons("pilihan", "Pilihan:",
                           choices = c("Tukar Baris", "Kalikan Baris", "Lipat Baris")),
              uiOutput("gauss_pilih")
            ),
            mainPanel(
              verbatimTextOutput("keterangan"),
              verbatimTextOutput("output")
            )
          )
        )
      )

      tes <<- gauss_jordan_manual(matrix_pers_a(),pers_vector_a())
      ket("Augmented matriks")
      hasil_verbatim(tes[1])
    } else{
      shinyalert(
        title = "Perhitungan Sedang Berlangsung",
        text = "Mohon tunggu...",
        type = "info",
        showCancelButton = FALSE,
        showConfirmButton = FALSE,
        timer = 3000
      )

      isolate({
        Sys.sleep(3)
        addClass(selector = "#step5_persamaan", class = "active")
        shinyalert(
          title = "Perhitungan Selesai",
          text = "Perhitungan telah selesai!",
          type = "success"
        )
      })
    }
  })

  output$gauss_pilih <- renderUI({
    if(input$pilihan == "Tukar Baris"){
      list(
        numericInput("baris_1", "Masukkan baris ke:", value = ""),
        numericInput("baris_2", "Tukar baris ke:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }
    else if(input$pilihan == "Kalikan Baris"){
      list(
        numericInput("baris_kali", "Masukkan baris ke:", value = ""),
        textInput("konstanta", "Dikalikan sebesar:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }
    else{
      list(
        numericInput("baris_lipat", "Masukkan baris tujuan:", value = ""),
        numericInput("baris_lipat_2", "Lipat dengan baris ke:", value = ""),
        textInput("konst_lipat", "Dengan kelipatan:", value = ""),
        actionButton("selesai", "Selesai")
      )
    }

  })


  observeEvent(input$selesai, {
    updateNumericInput(session, "baris_1", value = "")
    updateNumericInput(session, "baris_2", value = "")
    updateNumericInput(session, "baris_kali", value = "")
    updateTextInput(session, "konstanta", value = "")
    updateNumericInput(session, "baris_lipat", value = "")
    updateNumericInput(session, "baris_lipat_2", value = "")
    updateTextInput(session, "konst_lipat", value = "")



    if(input$pilihan == "Tukar Baris"){
      if(is.na(input$baris_1))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_1 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      if(is.na(input$baris_2))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_2 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      ket(paste0("Menukar baris ",input$baris_1, " dengan baris ", input$baris_2))
      hasil_verbatim(tes$tukar(input$baris_1, input$baris_2))

    }
    else if(input$pilihan == "Kalikan Baris"){

      if(is.na(input$baris_kali))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_kali > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      konstanta = 0
      if(input$konstanta == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu", type = "error"))
      else if(is_valid_fraction(input$konstanta)) konstanta = eval(parse(text = input$konstanta))
      else if(is.na(as.numeric(input$konstanta))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka", type = "error"))
      else konstanta = (as.numeric(input$konstanta))

      ket(paste0("Mengalikan baris ",input$baris_kali, " sebesar ", konstanta))
      hasil_verbatim(tes$kali(input$baris_kali,konstanta))

    }
    else{
      if(is.na(input$baris_lipat))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_lipat > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      if(is.na(input$baris_lipat_2))  return(shinyalert("Kesalahan Input", "Masukkan baris terlebih dahulu", type = "error"))
      else if(input$baris_lipat_2 > nrow(matrix_pers_a())) return(shinyalert("Kesalahan Input", "Baris tidak ada", type = "error"))

      konstanta_kali = 0
      if(input$konst_lipat == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu", type = "error"))
      else if(is_valid_fraction(input$konst_lipat)) konstanta_kali = eval(parse(text = input$konst_lipat))
      else if(is.na(as.numeric(input$konst_lipat))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka", type = "error"))
      else konstanta_kali = (as.numeric(input$konst_lipat))


      ket(paste0("Melipat baris ",input$baris_lipat, " dengan baris ",input$baris_lipat_2, " dikalikan ", konstanta_kali))
      hasil_verbatim(tes$lipat(input$baris_lipat_2,input$baris_lipat,konstanta_kali))
    }
  })

  output$output <- renderPrint({
    print(hasil_verbatim())
  })
  output$keterangan <- renderPrint({
    print(ket())
  })




  observeEvent(input$pers_reset_btn, {
    matrix_pers_a(NULL)
    pers_vector_a(NULL)
    hide_persamaan(session)
  })

  #### input matrix Hitung ####
  ### hitung 1

  output$submit_perhitungan <- renderUI({
    list(
      tags$div(style="color:red;", "*ukuran maksimal matriks yaitu 5x5"),
      actionButton("submit_1", "Next")
    )
  })

  matrix_hit_1 <- reactiveVal(NULL)
  observeEvent(input$submit_1, {
    if(is.na(input$hit_rows_1))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_rows_1 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_rows_1 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    if(is.na(input$hit_cols_1))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_cols_1 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_cols_1 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    addClass(selector = "#step2_perhitungan", class = "active")

    width <- paste0((input$hit_cols_1 *40),"px")
    output$hit_matrix_input_1 <- renderUI({
      list(
        h5(tags$b("Masukkan matriks pertama", id = "hit_matriks_1")),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "hit_input_matrix_1",
            value = matrix_hit_1(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })
    matrix_hit_1(matrix("", nrow = input$hit_rows_1, ncol = input$hit_cols_1, dimnames = list(NULL, NULL)))

    show("hit_submit_matrix_1")
    output$hit_submit_matrix_1 <-renderUI({
      list(
        div(style = "color:red","*input nilai matriks berupa angka"),
        actionButton("hit_submit_1", "Next")
      )
    })


    hide("submit_perhitungan")
    show("hit_matrix_input_1")

    output$hit_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("hit_reset_btn", "Reset")
      )
    })

  })
  matrix_hit_1_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_1, {
    mat <- c()
    for (i in 1:(input$hit_rows_1 * input$hit_cols_1)) {
      if(is.na(input$hit_input_matrix_1[[i]]))  return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$hit_input_matrix_1[[i]])
    }
    matrix_hit_1_a(matrix(mat, nrow = input$hit_rows_1, ncol = input$hit_cols_1))
    hide("hit_submit_matrix_1")

    addClass(selector = "#step3_perhitungan", class = "active")

    output$hit_output_matrix_1 <- renderPrint({
      matrix_hit_1_a()
    })

    show("hit_output_matrix_1")
    show("output_matrix_hit_2")
    show("error_hitung")
    output$output_matrix_hit_2 <- renderUI({
      list(h4("Matriks kedua"),
           numericInput("hit_rows_2", "Masukkan Jumlah Baris:", value = 2, min = 1, max=5),
           numericInput("hit_cols_2", "Masukkan Jumlah Kolom:", value = 2, min = 1, max=5),
           div(style="color:red;",id = "error_hitung", "*ukuran maksimal matriks yaitu 5x5"),
           actionButton("submit_2", "Next"))
    })
  })

  ### hitung 2
  matrix_hit_2 <- reactiveVal(NULL)
  observeEvent(input$submit_2, {

    if(is.na(input$hit_rows_2))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_rows_2 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_rows_2 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))


    if(is.na(input$hit_cols_2))  return(shinyalert("Kesalahan Input", "Masukkan ukuran terlebih dahulu", type = "error"))
    else if(input$hit_cols_2 > 5) return(shinyalert("Kesalahan Input", "Ukuran tidak bisa melebihi 5 x 5", type = "error"))
    else if(input$hit_cols_2 < 1) return(shinyalert("Kesalahan Input", "Ukuran minimal 1 x 1", type = "error"))

    addClass(selector = "#step4_perhitungan", class = "active")

    width <- paste0((input$hit_cols_2 *40),"px")
    output$hit_matrix_input_2 <- renderUI({
      list(
        h5(tags$b("Masukkan matriks kedua", id = "hit_matriks_2")),
        div(
          style = paste("width:", width),
          matrixInput(
            inputId = "hit_input_matrix_2",
            value = matrix_hit_2(),
            rows = list(extend = FALSE, names = FALSE),
            cols = list(extend = FALSE, names = FALSE),
            class = "numeric",
          )
        ))
    })

    matrix_hit_2(matrix("", nrow = input$hit_rows_2, ncol = input$hit_cols_2, dimnames = list(NULL, NULL)))

    hide("submit_2")
    hide("error_hitung")
    show("hit_matrix_input_2")
    show("hit_submit_matrix_2")
    output$hit_submit_matrix_2 <-renderUI({
      list(
        div(style = "color:red","*input nilai matriks berupa angka"),
        actionButton("hit_submit_2", "Next")
      )
    })
  })
  matrix_hit_2_a <- reactiveVal(NULL)
  observeEvent(input$hit_submit_2, {
    mat <- c()
    for (i in 1:(input$hit_rows_2 * input$hit_cols_2)) {
      if(is.na(input$hit_input_matrix_2[[i]]))  return(shinyalert("Kesalahan Input", "Masukkan matrix terlebih dahulu", type = "error"))
      mat <- c(mat, input$hit_input_matrix_2[[i]])
    }
    matrix_hit_2_a(matrix(mat, nrow = input$hit_rows_2, ncol = input$hit_cols_2))

    addClass(selector = "#step5_perhitungan", class = "active")
    hide("hit_submit_matrix_2")

    output$radio_hit <- renderUI(
      radioButtons("hit_radio",
                   label = "Pilih",
                   choices = c("Hitung", "Step Perkalian"),
                   selected = "none",
                   inline = TRUE))

    output$hit_output_matrix_2 <- renderPrint({
      matrix_hit_2_a()
    })

    show("hit_output_matrix_2")
  })

  observeEvent(input$hit_radio, {
    removeClass(selector = "#step6_perhitungan", class = "active")
    show("hitung_perh")
    hide("hitung")
    hide("hasil_perhitungan")
    output$hitung_perhitungan <- renderUI(
      actionButton("hitung_perh", "Hitung")
    )
  })

  observeEvent(input$hitung_perh,{
    hide("hitung_perh")
    show("hasil_perhitungan")
    output$hasil_perhitungan <- renderUI(
      verbatimTextOutput("hitung") %>% withSpinner()
    )
    show("hitung")

    output$hitung <- renderPrint(
      if(input$hit_radio == "Hitung"){
        hitung_matriks(matrix_hit_1_a(),matrix_hit_2_a())
      }
      else if(input$hit_radio == "Step Perkalian"){
        perkalian_matriks_dengan_langkah(matrix_hit_1_a(),matrix_hit_2_a())
      }
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 3000
    )

    isolate({
      Sys.sleep(3)
      addClass(selector = "#step6_perhitungan", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

  })

  observeEvent(input$hit_reset_btn, {
    matrix_hit_1_a(NULL)
    matrix_hit_2_a(NULL)
    hide_hitung(session)
  })


  #### input vector hitung ####

  output$submit_perhitungan_vector<-renderUI({
    list(
      div(style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
      actionButton("hitvec_submit_1", "Next")
    )
  })
  hit_vector <- reactiveVal(NULL)
  observeEvent(input$hitvec_submit_1, {
    vector_hit <- as.numeric(strsplit(input$hit_vec, ",")[[1]])

    if(length(vector_hit) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_hit)){
      if(is.na(vector_hit[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_hit)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_hit)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    hit_vector(vector_hit)


    addClass(selector = "#step2_hitvec", class = "active")
    hide("submit_perhitungan_vector")
    output$hitvec_output_vector_1 <- renderPrint({
      hit_vector()
    })
    show("hitvec_output_vector_1")
    show("output_hitvec_2")
    show("dotcross_reset")

    show("hitvec_reset")

    output$hitvec_reset <- renderUI({
        div(
        class = "reset-button",
        actionButton("hitvec_reset_btn", "Reset")
      )
    })

    show("error_hitvec")
    output$output_hitvec_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("hit_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_hitvec",style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
        actionButton("hitvec_submit_2", "Next")
      )
    })
  })

  hit_vector_2 <- reactiveVal(NULL)
  observeEvent(input$hitvec_submit_2, {
    vector_hit_2 <- as.numeric(strsplit(input$hit_vec_2, ",")[[1]])
    if(length(vector_hit_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_hit_2)){
      if(is.na(vector_hit_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(hit_vector())!=length(vector_hit_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    addClass(selector = "#step3_hitvec", class = "active")

    hide("hitvec_submit_2")
    hide("error_hitvec")
    show("hitvec_output_vector_2")
    hit_vector_2(vector_hit_2)
    output$hitvec_output_vector_2 <- renderPrint({
      hit_vector_2()
    })


    output$radio_hitvec <- renderUI(
      radioButtons("hitvec_radio",
                   label = "Pilih metode",
                   choices = c("Tambah", "Kurang"),
                   selected = "none",
                   inline = TRUE)
    )
  })

  observeEvent(input$hitvec_radio, {
    hide("hitvec")
    hide("plot_hitung_ui")
    show("hitung_hitvector")
    removeClass(selector = "#step4_hitvec", class = "active")

    output$hitung_hitvec <- renderUI(
      actionButton("hitung_hitvector", "Hitung")
    )
  })

  observeEvent(input$hitung_hitvector,{

    hide("hitung_hitvector")
    show("plot_hitung_ui")
    show("hitvec")

    output$plot_hitung_ui <- renderUI({
      div(
        verbatimTextOutput("hitvec") %>% withSpinner(),
        plotlyOutput("plot_hitung") %>% withSpinner()
      )
    })
    show("plot_hitung")

    output$plot_hitung <- renderPlotly({
      if(input$hitvec_radio == "Tambah"){
        tambah_vektor(hit_vector(),hit_vector_2())
      }
      else if(input$hitvec_radio == "Kurang"){
        kurang_vektor(hit_vector(),hit_vector_2())
      }
    })

    output$hitvec <- renderPrint(
      if(input$hitvec_radio == "Tambah"){
        tambah_vektor(hit_vector(),hit_vector_2())
      }
      else if(input$hitvec_radio == "Kurang"){
        kurang_vektor(hit_vector(),hit_vector_2())
      }
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_hitvec", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })
  })



  observeEvent(input$hitvec_reset_btn, {
    hit_vector(NULL)
    hit_vector_2(NULL)
    hide_hitvec(session)
  })

  #### input proyeksi #####

  output$submit_proyeksi <- renderUI({
    list(
      div(style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
      actionButton("proyeksi_submit_1", "Next")
    )
  })
  proyeksi_vector <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit_1, {

    vector_proyeksi <- as.numeric(strsplit(input$proyeksi_vec, ",")[[1]])

    if(length(vector_proyeksi) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_proyeksi)){
      if(is.na(vector_proyeksi[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_proyeksi)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_proyeksi)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    proyeksi_vector(vector_proyeksi)

    addClass(selector = "#step2_proyeksi", class = "active")

    hide("submit_proyeksi")
    output$proyeksi_output_vector_1 <- renderPrint({
      proyeksi_vector()
    })
    show("proyeksi_output_vector_1")
    show("output_proyeksi_2")
    show("proyeksi_reset")

    output$proyeksi_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("proyeksi_reset_btn", "Reset")
      )
    })

    show("error_proyeksi")
    output$output_proyeksi_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("proyeksi_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_proyeksi",style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
        actionButton("proyeksi_submit_2", "Next")
      )
    })
  })

  proyeksi_vector_2 <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit_2, {
    vector_proyeksi_2 <- as.numeric(strsplit(input$proyeksi_vec_2, ",")[[1]])
    if(length(vector_proyeksi_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_proyeksi_2)){
      if(is.na(vector_proyeksi_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(proyeksi_vector())!=length(vector_proyeksi_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    addClass(selector = "#step3_proyeksi", class = "active")

    hide("proyeksi_submit_2")
    hide("error_proyeksi")
    show("proyeksi_output_vector_2")
    proyeksi_vector_2(vector_proyeksi_2)
    output$proyeksi_output_vector_2 <- renderPrint({
      proyeksi_vector_2()
    })


    output$radio_proyeksi <- renderUI(
      radioButtons("proyeksi_radio",
                   label = "Pilih metode",
                   choices = c("Norm", "Proyeksi Vektor"),
                   selected = "none",
                   inline = TRUE)
    )
  })

  observeEvent(input$proyeksi_radio, {

    removeClass(selector = "#step4_proyeksi", class = "active")

    show("hitung_proyeksi")
    hide("proyeksi")
    hide("proyeksi_2")
    hide("plot_proyeksi_ui")
    output$hitung_proyeksi <- renderUI(
      actionButton("hitung_proy", "Hitung")
    )
  })

  observeEvent(input$hitung_proy,{

    hide("hitung_proyeksi")
    show("plot_proyeksi_ui")
    show("proyeksi")

    output$plot_proyeksi_ui <- renderUI({
      div(
        verbatimTextOutput("proyeksi") %>% withSpinner(),
        verbatimTextOutput("proyeksi_2")  %>% withSpinner(),
        plotlyOutput("plot_proyeksi") %>% withSpinner()
      )
    }

    )

    show("proyeksi_2")
    show("plot_proyeksi")
    output$plot_proyeksi <- renderPlotly({
      if(input$proyeksi_radio == "Proyeksi Vektor"){
        proyeksi_vektor(proyeksi_vector(),proyeksi_vector_2())
      }
    })

    output$proyeksi <- renderPrint(
      if(input$proyeksi_radio == "Norm"){
        norm_vektor(proyeksi_vector())
      }
      else if(input$proyeksi_radio == "Proyeksi Vektor"){
        proyeksi_vektor(proyeksi_vector(),proyeksi_vector_2())
      }
    )
    output$proyeksi_2<-renderPrint(
      if(input$proyeksi_radio == "Norm"){
        norm_vektor(proyeksi_vector_2())
      }
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_proyeksi", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

  })

  observeEvent(input$proyeksi_reset_btn, {
    proyeksi_vector(NULL)
    proyeksi_vector_2(NULL)
    hide_proyeksi(session)
  })

  #### input dot product dan cross product ####

  output$submit_dotcross <- renderUI({
    list(
      div(style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
      actionButton("dot_submit_1", "Next")
    )
  })
  dot_vector <- reactiveVal(NULL)
  observeEvent(input$dot_submit_1, {

    vector_dot <- as.numeric(strsplit(input$dot_vec, ",")[[1]])

    if(length(vector_dot) == 0) return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_dot)){
      if(is.na(vector_dot[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_dot)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_dot)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))
    dot_vector(vector_dot)

    addClass(selector = "#step2_dotcross", class = "active")

    hide("submit_dotcross")
    output$dot_output_vector_1 <- renderPrint({
      dot_vector()
    })
    show("dot_output_vector_1")
    show("output_dot_2")
    show("dotcross_reset")

    output$dotcross_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dotcross_reset_btn", "Reset")
      )
    })

    show("error_dotcross")
    output$output_dot_2 <- renderUI({
      list(
        h5(tags$b("Masukkan vektor kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("dot_vec_2",label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_dotcross",style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
        actionButton("dot_submit_2", "Next")
      )
    })
  })

  dot_vector_2 <- reactiveVal(NULL)
  observeEvent(input$dot_submit_2, {
    vector_dot_2 <- as.numeric(strsplit(input$dot_vec_2, ",")[[1]])
    if(length(vector_dot_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_dot_2)){
      if(is.na(vector_dot_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(dot_vector())!=length(vector_dot_2)) return(shinyalert("Kesalahan Input", "Banyaknya vektor kedua harus sama dengan vektor pertama", type = "error"))

    addClass(selector = "#step3_dotcross", class = "active")

    hide("dot_submit_2")
    show("dot_output_vector_2")
    hide("error_dotcross")
    dot_vector_2(vector_dot_2)
    output$dot_output_vector_2 <- renderPrint({
      dot_vector_2()
    })


    output$radio_dotcross <- renderUI(
      radioButtons("dotcross_radio",
                   label = "Pilih metode",
                   choices = c("Dot Product", "Cross Product"),
                   selected = "none",
                   inline = TRUE)
    )
  })

  observeEvent(input$dotcross_radio, {

    removeClass(selector = "#step4_dotcross", class = "active")

    show("hitung_dotcross")
    hide("dotcross")
    hide("plot_cross_ui")

    output$hitung_dotcross <- renderUI(
      actionButton("hitung_crossdot","Hitung")
    )
  })

  observeEvent(input$hitung_crossdot,{

    show("plot_cross_ui")
    hide("hitung_crossdot")
    show("dotcross")

    output$plot_cross_ui <- renderUI({
      div(
        verbatimTextOutput("dotcross") %>% withSpinner(),
        plotlyOutput("plot_cross") %>% withSpinner()
      )
    })

    show("plot_cross")
    output$plot_cross <- renderPlotly({
      if(input$dotcross_radio == "Dot Product"){
        dot_product(dot_vector(), dot_vector_2())
      }
      else if(input$dotcross_radio == "Cross Product"){
        if(length(dot_vector()) == 3) cross_product(dot_vector(), dot_vector_2())
      }
    })
    output$dotcross <- renderPrint(
      if(input$dotcross_radio == "Dot Product"){
        dot_product(dot_vector(), dot_vector_2())
      }
      else if(input$dotcross_radio == "Cross Product"){
        if(length(dot_vector()) == 2) "Cross product tidak dapat dilakukan pada vektor 2 dimensi"
        else cross_product(dot_vector(), dot_vector_2())
      }
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_dotcross", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })
  })

  observeEvent(input$dotcross_reset_btn, {
    dot_vector(NULL)
    dot_vector_2(NULL)
    hide_dotcross(session)
  })


  #### intput Polinomial ####

  output$submit_polinomial <- renderUI({
    list(
      div(style = "color:red","*input nilai koordinat x dengan angka (dipisahkan dengan koma)"),
      actionButton("x_submit", "Next")
    )
  })

  x_vector <- reactiveVal(NULL)
  observeEvent(input$x_submit, {
    vector_x <- as.numeric(strsplit(input$x_vec, ",")[[1]])

    if(length(vector_x) == 0)return(shinyalert("Kesalahan Input", "Masukkan koordinat x terlebih dahulu", type = "error"))
    for(i in 1:length(vector_x)){
      if(is.na(vector_x[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(vector_x)< 2) return(shinyalert("Kesalahan Input", "Banyaknya koordinat x minimal 2 elemen", type = "error"))

    addClass(selector = "#step2_polinomial", class = "active")

    x_vector(vector_x)
    hide("submit_polinomial")

    output$x_output_vector <- renderPrint({
      x_vector()
    })

    show("x_output_vector")
    show("output_koor_y")
    show("poli_reset")

    output$poli_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("poli_reset_btn", "Reset")
      )
    })

    show("error_polinomial")
    output$output_koor_y <- renderUI({
      list(
        h5(tags$b("Masukkan koordinat y (y1, y2, y3, ..., yn)")),
        textInput("y_vec", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_polinomial",style = "color:red","*input nilai koordinat y berupa angka (dipisahkan dengan koma) sebanyak jumlah koordinat x"),
        actionButton("y_submit", "Next")
      )
    })
  })

  y_vector <- reactiveVal(NULL)
  observeEvent(input$y_submit, {
    vector_y <- as.numeric(strsplit(input$y_vec, ",")[[1]])

    if(length(vector_y) == 0)return(shinyalert("Kesalahan Input", "Masukkan koordinat y terlebih dahulu", type = "error"))
    for(i in 1:length(vector_y)){
      if(is.na(vector_y[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }
    if(length(vector_y)!=length(x_vector())) return(shinyalert("Kesalahan Input", "Banyaknya vektor y harus sama dengan vektor x", type = "error"))

    hide("y_submit")
    hide("error_polinomial")
    show("y_output_vector")

    addClass(selector = "#step3_polinomial", class = "active")

    y_vector(vector_y)
    output$y_output_vector <- renderPrint({
      y_vector()
    })

    show("hitung_poli")
    output$hitung_poli <- renderUI(actionButton("poli_submit", "Hitung Interpolasi"))
  })

  observeEvent(input$poli_submit, {
    show("hasil_poli")

    output$hasil_poli <- renderUI(
      verbatimTextOutput("poli") %>% withSpinner()
    )

    hide("poli_submit")
    output$poli <- renderPrint(
      interpolasi_polinomial(x_vector(),y_vector())
    )
    show("poli")

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 3000
    )

    isolate({
      Sys.sleep(3)
      addClass(selector = "#step4_polinomial", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })
  })

  observeEvent(input$poli_reset_btn, {
    x_vector(NULL)
    y_vector(NULL)
    hide_interpolasi(session)

  })



  #### input distance ####
  observeEvent(input$dist_radio,{
    hide("distance")
    hide("plot_distance_ui")
    hide("hitung_distance")
    hide("bidang_output_vector")
    hide("bidang_output_vector_2")
    hide("input_vector_dist")
    hide("titik_output_vector_2")
    hide("input_vector_dist_2")
    hide("titik_output_vector_1")
    hide("bidang_output_vector_1")
    hide("titik_output_vector")
    removeClass(selector = "#step5_distance", class = "active")
    removeClass(selector = "#step4_distance", class = "active")
    removeClass(selector = "#step3_distance", class = "active")
    removeClass(selector = "#step2_distance", class = "active")
    addClass(selector = "#step1_distance", class = "active")
    show("error_distance")
    show("hitung_distances")

    output$hitung_distances <- renderUI(
      actionButton("hitung_dist","Next")
    )
  })

  observeEvent(input$hitung_dist,{
    hide("hitung_dist")
    addClass(selector = "#step2_distance", class = "active")
    show("input_vector_dist")
    output$input_vector_dist <- renderUI(
      if (input$dist_radio == "Jarak 2 titik") {
        list(
          h5(tags$b("Masukkan titik pertama ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
          textInput("vec_titik_1", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
          div(id="error_distance",style = "color:red","*input nilai titik berupa angka (dipisahkan dengan koma)"),
          actionButton("titik_submit_1", "Next")
        )
      } else if (input$dist_radio == "Jarak titik ke garis") {
        list(
          h5(tags$b("Masukkan titik ((x,y) karena untuk vektor 2 dimensi")),
          textInput("vec_bidang_1", label = NULL, width = "30%", placeholder = "contoh : 1,2"),
          div(id="error_distance",style = "color:red","*input nilai titik berupa angka (dipisahkan dengan koma)"),
          actionButton("bidang_submit_1", "Next")
        )
      } else {
        list(
          h5(tags$b("Masukkan titik ((x,y,z) karena untuk vektor 3 dimensi)")),
          textInput("vec_titik", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
          div(id="error_distance",style = "color:red","*input nilai titik berupa angka (dipisahkan dengan koma)"),
          actionButton("titik_submit", "Next")
        )
      }
    )

    output$dist_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("dist_reset_btn", "Reset")
      )
    })
  })

  ### 2 titik
  titik_vector_1 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_1, {

    vector_titik_1 <- as.numeric(strsplit(input$vec_titik_1, ",")[[1]])

    if(length(vector_titik_1) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik_1)){
      if(is.na(vector_titik_1[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik_1)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_titik_1)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))

    hide("titik_submit_1")
    hide("error_distance")

    addClass(selector = "#step3_distance", class = "active")

    titik_vector_1(vector_titik_1)
    output$titik_output_vector_1 <- renderPrint({
      titik_vector_1()
    })

    show("titik_output_vector_1")
    show("input_vector_dist_2")
    show("error_distance_2")

    output$input_vector_dist_2 <- renderUI({
      list(
        h5(tags$b("Masukkan titik kedua ((x,y) untuk vektor 2 dimensi / (x,y,z) untuk vektor 3 dimensi)")),
        textInput("vec_titik_2", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_distance_2",style = "color:red","*input nilai titik berupa angka (dipisahkan dengan koma)"),
        actionButton("titik_submit_2", "Next")
      )
    })
  })

  bidang_vector_1 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_1, {
    vector_bidang_1 <- as.numeric(strsplit(input$vec_bidang_1, ",")[[1]])

    if(length(vector_bidang_1) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang_1)){
      if(is.na(vector_bidang_1[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang_1) != 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor harus 2 elemen", type = "error"))

    hide("bidang_submit_1")
    hide("error_distance")


    addClass(selector = "#step3_distance", class = "active")

    bidang_vector_1(vector_bidang_1)
    output$bidang_output_vector_1 <- renderPrint({
      bidang_vector_1()
    })


    show("bidang_output_vector_1")
    show("input_vector_dist_2")
    show("error_distance_2")


    output$input_vector_dist_2 <- renderUI({
      list(
        h5(tags$b("Masukkan koefisien garis ((a,b,c) dari persamaan ax +by + c = 0 ))")),
        textInput("vec_bidang_2", label = NULL, width = "30%", placeholder = "contoh : 1,2,3"),
        div(id="error_distance_2",style = "color:red","*input nilai garis berupa angka (dipisahkan dengan koma)"),
        actionButton("bidang_submit_2", "Next")
      )
    })
  })

  titik_vector <- reactiveVal(NULL)
  observeEvent(input$titik_submit, {
    vector_titik <- as.numeric(strsplit(input$vec_titik, ",")[[1]])

    if(length(vector_titik) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik)){
      if(is.na(vector_titik[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik) != 3) return(shinyalert("Kesalahan Input", "Banyaknya vektor harus 3 elemen", type = "error"))

    hide("titik_submit")
    hide("error_distance")

    addClass(selector = "#step3_distance", class = "active")


    titik_vector(vector_titik)
    output$titik_output_vector <- renderPrint({
      titik_vector()
    })


    show("titik_output_vector")
    show("input_vector_dist_2")
    show("error_distance_2")


    output$input_vector_dist_2 <- renderUI({
      list(

        h5(tags$b("Masukkan koefisien bidang ((a,b,c,d) dari persamaan ax +by + cz + d = 0 )")),
        textInput("vec_bidang", label = NULL, width = "30%", placeholder = "contoh : 1,2,3,4"),
        div(id="error_distance_2",style = "color:red","*input nilai bidang berupa angka (dipisahkan dengan koma)"),
        actionButton("bidang_submit", "Next")
      )
    })

  })

  titik_vector_2 <- reactiveVal(NULL)
  observeEvent(input$titik_submit_2, {
    vector_titik_2 <- as.numeric(strsplit(input$vec_titik_2, ",")[[1]])

    if(length(vector_titik_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_titik_2)){
      if(is.na(vector_titik_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_titik_2) != length(titik_vector_1())) return(shinyalert("Kesalahan Input", "Banyaknya vektor titik kedua harus sama dengan vektor titik pertama", type = "error"))

    hide("titik_submit_2")
    hide("error_distance_2")
    addClass(selector = "#step4_distance", class = "active")


    titik_vector_2(vector_titik_2)
    output$titik_output_vector_2 <- renderPrint({
      titik_vector_2()
    })

    show("titik_output_vector_2")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$titik_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector_2 <- reactiveVal(NULL)
  observeEvent(input$bidang_submit_2, {
    vector_bidang_2 <- as.numeric(strsplit(input$vec_bidang_2, ",")[[1]])

    if(length(vector_bidang_2) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang_2)){
      if(is.na(vector_bidang_2[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang_2) != 3) return(shinyalert("Kesalahan Input", "Banyaknya vektor garis harus 3 elemen", type = "error"))

    hide("bidang_submit_2")
    hide("error_distance_2")
    addClass(selector = "#step4_distance", class = "active")


    bidang_vector_2(vector_bidang_2)
    output$bidang_output_vector_2 <- renderPrint({
      bidang_vector_2()
    })
    show("bidang_output_vector_2")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$bidang_submit_2>0) actionButton("distance_submit", "Hitung Jarak"))
  })

  bidang_vector <- reactiveVal(NULL)
  observeEvent(input$bidang_submit, {
    addClass(selector = "#step4_distance", class = "active")
    vector_bidang <- as.numeric(strsplit(input$vec_bidang, ",")[[1]])

    if(length(vector_bidang) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_bidang)){
      if(is.na(vector_bidang[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_bidang) != 4) return(shinyalert("Kesalahan Input", "Banyaknya vektor bidang harus 4 elemen", type = "error"))

    hide("bidang_submit")
    hide("error_distance_2")

    addClass(selector = "#step4_distance", class = "active")

    bidang_vector(vector_bidang)
    output$bidang_output_vector <- renderPrint({
      bidang_vector()
    })

    show("bidang_output_vector")
    show("hitung_distance")
    output$hitung_distance <- renderUI(if(input$bidang_submit>0) actionButton("distance_submit", "Hitung Jarak"))
  })


  observeEvent(input$distance_submit, {
    addClass(selector = "#step5_distance", class = "active")
    show("plot_distance_ui")

    output$plot_distance_ui <- renderUI({
      div(
        verbatimTextOutput("distance") %>% withSpinner(),
        plotlyOutput("plot_distance") %>% withSpinner()
      )
    })

    show("plot_distance")
    if (input$dist_radio == "Jarak 2 titik") {
      if(input$distance_submit>0){
        output$plot_distance <- renderPlotly(
          distance_dua_titik(titik_vector_1(),titik_vector_2())
        )
      }
    } else if (input$dist_radio == "Jarak titik ke garis") {
      output$plot_distance <- renderPlotly(
        distance_point_line(bidang_vector_1(),bidang_vector_2())
      )
    } else {
      output$plot_distance <- renderPlotly(
        distance_point_plane(titik_vector(),bidang_vector())
      )
    }


    hide("distance_submit")
    show("distance")
    if (input$dist_radio == "Jarak 2 titik") {
      if(input$distance_submit>0){
        output$distance <- renderPrint(
          distance_dua_titik(titik_vector_1(),titik_vector_2())
        )
      }
    } else if (input$dist_radio == "Jarak titik ke garis") {
      output$distance <- renderPrint(
        distance_point_line(bidang_vector_1(),bidang_vector_2())
      )
    } else {
      output$distance <- renderPrint(
        distance_point_plane(titik_vector(),bidang_vector())
      )
    }

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step6_distance", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })
  })


  observeEvent(input$dist_reset_btn, {
    updateNumericInput(session, "vec_titik_1", value = "")
    updateNumericInput(session, "vec_bidang_1", value = "")
    updateNumericInput(session, "vec_titik", value = "")
    show("hitung_dist")
    hide("input_vector_dist")
    hide("distance")
    hide("hitung_distance")
    hide("plot_distance_ui")
    hide("bidang_output_vector")
    hide("bidang_output_vector_2")
    hide("titik_output_vector_2")
    hide("input_vector_dist_2")
    hide("titik_output_vector_1")
    hide("bidang_output_vector_1")
    hide("titik_output_vector")
    hide("dist_reset_btn")
    show("titik_submit_1")
    show("bidang_submit_1")
    removeClass(selector = "#step5_distance", class = "active")
    removeClass(selector = "#step4_distance", class = "active")
    removeClass(selector = "#step3_distance", class = "active")
    removeClass(selector = "#step2_distance", class = "active")
    addClass(selector = "#step1_distance", class = "active")
    show("titik_submit")
  })

  #### input transformasi linear ####

  output$submit_transformasi <- renderUI({
    list(
      div(style = "color:red","*input nilai vektor berupa angka (dipisahkan dengan koma)"),
      actionButton("trans_submit", "Next")
    )
  })

  trans_vector <- reactiveVal(NULL)
  observeEvent(input$trans_submit, {
    vector_trans <- as.numeric(strsplit(input$trans_vec, ",")[[1]])

    if(length(vector_trans) == 0)return(shinyalert("Kesalahan Input", "Masukkan vektor terlebih dahulu", type = "error"))
    for(i in 1:length(vector_trans)){
      if(is.na(vector_trans[[i]])) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input bukan angka", type = "error"))
    }

    if(length(vector_trans)>3) return(shinyalert("Kesalahan Input", "Banyaknya vektor maksimal 3 elemen", type = "error"))
    else if(length(vector_trans)< 2) return(shinyalert("Kesalahan Input", "Banyaknya vektor minimal 2 elemen", type = "error"))

    addClass(selector = "#step2_transformasi", class = "active")
    hide("submit_transformasi")

    trans_vector(vector_trans)
    output$trans_output_vector <- renderPrint({
      trans_vector()
    })

    show("trans_output_vector")
    show("radio_trans")

    output$trans_reset <- renderUI({
      div(
        class = "reset-button",
        actionButton("trans_reset_btn", "Reset")
      )
    })

    output$radio_trans <- renderUI(radioButtons("trans_radio",
                                                label = "Pilih metode",
                                                choices = c("Refleksi", "Rotasi", "Proyeksi", "Kontraksi/Dilatasi"),
                                                selected = "none",
                                                inline = TRUE))
  })

  observeEvent(input$trans_radio,{

    hide("input_info_trans")
    hide("refleksi_output_vector")
    hide("proyeksi_output_vector")
    hide("kontraksi_output_vector")
    hide("rotasi_output_vector")
    hide("rotasi_output_vector_alfa")
    hide("rotasi_axis_input")
    hide("plot")
    hide("transformasi")

    removeClass(selector = "#step3_transformasi", class = "active")
    removeClass(selector = "#step4_transformasi", class = "active")

    output$hitung_transformasi <- renderUI(
      actionButton("hitung_trans","Next")
    )
  })

  observeEvent(input$hitung_trans,{
    addClass(selector = "#step3_transformasi", class = "active")

    hide("hitung_trans")
    show("input_info_trans")
    show("error_transformasi")
    output$input_info_trans <- renderUI({
      if (input$trans_radio == "Refleksi") {
        list(
          if(length(trans_vector()) == 2){
            list(
              h5(tags$b("Masukkan terhadap axis (x / y / line)")),
              textInput("vec_refleksi", label = NULL, width = "30%"),
              div(id="error_transformasi",style = "color:red","*input diisi dengan 'x', 'y', atau 'line'"),
              actionButton("refleksi_submit", "Hitung")
            )
          }

          else {
            list(
              h5(tags$b("Masukkan terhadap bidang (xy / xz / yz)")),
              textInput("vec_refleksi", label = NULL, width = "30%"),
              div(id="error_transformasi",style = "color:red","*input diisi dengan 'xy', 'xz', atau 'yz'"),
              actionButton("refleksi_submit", "Hitung")
            )
          }
        )
      } else if (input$trans_radio == "Rotasi") {
        list(
          h5(tags$b("Besar Sudut rotasi (dalam satuan derajat)")),
          textInput("vec_rotasi_alfa", label = NULL, width = "30%"),
          div(id="error_transformasi",style = "color:red","*input diisi dengan angka"),
          actionButton("rotasi_submit_alfa", ifelse(length(trans_vector()) == 2,"Hitung","Next"))
        )
      } else if(input$trans_radio == "Proyeksi"){
        list(
          if(length(trans_vector()) == 2){
            list(
              h5(tags$b("Masukkan terhadap axis (x / y)")),
              textInput("vec_proyeksi", label = NULL, width = "30%"),
              div(id="error_transformasi",style = "color:red","*input diisi dengan 'x' atau 'y'"),
              actionButton("proyeksi_submit", "Hitung")
            )
          }
          else {
            list(
              h5(tags$b("Masukkan terhadap bidang (xy / xz / yz)")),
              textInput("vec_proyeksi", label = NULL, width = "30%"),
              div(id="error_transformasi",style = "color:red","*input diisi dengan 'xy', 'xz', atau 'yz'"),
              actionButton("proyeksi_submit", "Hitung")
            )
          }
        )
      } else {
        list(
          h5(tags$b("Masukkan besaran skalar")),
          textInput("vec_kontraksi", label = NULL, width = "30%"),
          div(id="error_transformasi",style = "color:red","*input diisi dengan angka"),
          actionButton("kontraksi_submit", "Hitung")
        )
      }
    })

  })

  refleksi_vector <- reactiveVal(NULL)
  observeEvent(input$refleksi_submit, {

    vector_ref <- input$vec_refleksi

    if(vector_ref == "") return(shinyalert("Kesalahan Input", ifelse(length(trans_vector()) == 2, "Masukkan axis terlebih dahulu", "Masukkan bidang terlebih dahulu"), type = "error"))
    else if (!(vector_ref %in%  c("x","y","line")) && length(trans_vector()) == 2) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', 'y', atau 'line'", type = "error"))
    }
    else if (!(vector_ref %in%  c("xy","xz","yz")) && length(trans_vector()) == 3) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'xy', 'xz', atau 'yz'", type = "error"))
    }


    hide("refleksi_submit")
    hide("error_transformasi")
    refleksi_vector(vector_ref)
    output$refleksi_output_vector <- renderPrint({
      refleksi_vector()
    })

    show("refleksi_output_vector")

    output$transformasi <- renderPrint(
      reflection_vector(trans_vector(),refleksi_vector())
    )

    show("plot_ui")

    output$plot_ui <- renderUI({
      div(
        verbatimTextOutput("transformasi") %>% withSpinner(),
        plotlyOutput("plot") %>% withSpinner()
      )
    })

    output$plot <- renderPlotly({
      reflection_vector(trans_vector(),refleksi_vector())
    })

    show("plot")
    show("transformasi")

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_transformasi", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })
  })

  proyeks_vector <- reactiveVal(NULL)
  observeEvent(input$proyeksi_submit, {

    vector_pro <- input$vec_proyeksi

    if(vector_pro == "") return(shinyalert("Kesalahan Input", ifelse(length(trans_vector()) == 2, "Masukkan axis terlebih dahulu", "Masukkan bidang terlebih dahulu"), type = "error"))
    else if (!(vector_pro %in%  c("x","y")) && length(trans_vector()) == 2) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', atau 'y", type = "error"))
    }
    else if (!(vector_pro %in%  c("xy","xz","yz")) && length(trans_vector()) == 3) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'xy', 'xz', atau 'yz'", type = "error"))
    }

    hide("proyeksi_submit")
    hide("error_transformasi")

    proyeks_vector(vector_pro)
    output$proyeksi_output_vector <- renderPrint({
      proyeks_vector()
    })

    show("proyeksi_output_vector")

    show("plot_ui")

    output$plot_ui <- renderUI({
      div(
        verbatimTextOutput("transformasi") %>% withSpinner(),
        plotlyOutput("plot") %>% withSpinner()
      )
    })
    output$plot <- renderPlotly({
      projection_vector(trans_vector(),proyeks_vector())
    })

    output$transformasi <- renderPrint(
      projection_vector(trans_vector(),proyeks_vector())
    )
    show("plot")
    show("transformasi")
    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_transformasi", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })



  })

  kontraksi_vector <- reactiveVal(NULL)
  observeEvent(input$kontraksi_submit, {


    vector_kont <- input$vec_kontraksi

    if(vector_kont == "") return(shinyalert("Kesalahan Input", "Masukkan angka terlebih dahulu",type = "error"))
    else if(is_valid_fraction(vector_kont)) kontraksi_vector(eval(parse(text = vector_kont)))
    else if(is.na(as.numeric(vector_kont))) return(shinyalert("Kesalahan Input", "Terjadi kesalahan input bukan angka",type = "error"))
    else kontraksi_vector(as.numeric(vector_kont))

    hide("kontraksi_submit")
    output$kontraksi_output_vector <- renderPrint({
      kontraksi_vector()
    })

    show("kontraksi_output_vector")
    hide("error_transformasi")

    show("plot_ui")

    output$plot_ui <- renderUI({
      div(
        verbatimTextOutput("transformasi") %>% withSpinner(),
        plotlyOutput("plot") %>% withSpinner()
      )
    })
    output$plot <- renderPlotly({
      contraction_dilatation(trans_vector(),kontraksi_vector())
    })

    output$transformasi <- renderPrint(
       contraction_dilatation(trans_vector(),kontraksi_vector())
    )

    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_transformasi", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

    show("plot")
    show("transformasi")
  })

  rotasi_vector_alfa <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit_alfa, {
    vector_rot_alfa <- as.numeric(input$vec_rotasi_alfa)

    if(is.na(vector_rot_alfa)) return(shinyalert("Kesalahan Input", "Terdapat kesalahan input", type = "error"))

    hide("rotasi_submit_alfa")
    rotasi_vector_alfa(vector_rot_alfa)
    output$rotasi_output_vector_alfa <- renderPrint({
      rotasi_vector_alfa()
    })

    show("rotasi_output_vector_alfa")
    hide("error_transformasi")

    if(length(trans_vector()) == 2){

      show("plot_ui")

      output$plot_ui <- renderUI({
        div(
          verbatimTextOutput("transformasi") %>% withSpinner(),
          plotlyOutput("plot") %>% withSpinner()
        )
      })

      output$plot <- renderPlotly({
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
      })

      output$transformasi <- renderPrint(
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
      )
      show("plot")
      show("transformasi")

      shinyalert(
        title = "Perhitungan Sedang Berlangsung",
        text = "Mohon tunggu...",
        type = "info",
        showCancelButton = FALSE,
        showConfirmButton = FALSE,
        timer = 5000
      )

      isolate({
        Sys.sleep(5)
        addClass(selector = "#step4_transformasi", class = "active")
        shinyalert(
          title = "Perhitungan Selesai",
          text = "Perhitungan telah selesai!",
          type = "success"
        )
      })

    }
    else{
      show("rotasi_axis_input")
      hide("plot_ui")

      show("error_transformasi_2")
      output$rotasi_axis_input <- renderUI(
        list(
          h5(tags$b("Masukkan terhadap axis (x / y / z)")),
          textInput("vec_rotasi", label = NULL, width = "30%"),
          div(id="error_transformasi_2",style = "color:red","*input diisi dengan 'x', 'y', atau 'z'"),
          actionButton("rotasi_submit", "Hitung")
        )
      )
    }


  })

  rotasi_vector <- reactiveVal(NULL)
  observeEvent(input$rotasi_submit, {
    vector_rot <- input$vec_rotasi

    if(vector_rot == "") return(shinyalert("Kesalahan Input", "Masukkan axis terlebih dahulu", type = "error"))
    else if (!(vector_rot %in%  c("x","y","z")) ) {
      return(shinyalert("Kesalahan Input", "Pilih antara 'x', 'y', atau 'z'", type = "error"))
    }

    hide("rotasi_submit")
    hide("error_transformasi_2")

    rotasi_vector(vector_rot)
    output$rotasi_output_vector <- renderPrint({
      rotasi_vector()
    })

    show("rotasi_output_vector")

    show("plot_ui")

    output$plot_ui <- renderUI({
      div(
        verbatimTextOutput("transformasi")%>% withSpinner(),
        plotlyOutput("plot") %>% withSpinner()
      )
    })

    output$plot <- renderPlotly({
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    })

    output$transformasi <- renderPrint(
        rotation_vector(trans_vector(),rotasi_vector_alfa(),rotasi_vector())
    )
    shinyalert(
      title = "Perhitungan Sedang Berlangsung",
      text = "Mohon tunggu...",
      type = "info",
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      timer = 5000
    )

    isolate({
      Sys.sleep(5)
      addClass(selector = "#step4_transformasi", class = "active")
      shinyalert(
        title = "Perhitungan Selesai",
        text = "Perhitungan telah selesai!",
        type = "success"
      )
    })

    show("plot")
    show("transformasi")


  })

  observeEvent(input$trans_reset_btn, {
    refleksi_vector(NULL)
    trans_vector(NULL)
    updateTextInput(session, "trans_vec", value = "")
    hide("trans_reset_btn")
    hide("transformasi")
    hide("plot_ui")
    hide("plot")
    hide("refleksi_output_vector")
    hide("proyeksi_output_vector")
    hide("kontraksi_output_vector")
    hide("rotasi_output_vector")
    hide("rotasi_output_vector_alfa")
    hide("rotasi_axis_input")
    hide("trans_output_vector")
    hide("trans_radio")
    hide("input_info_trans")
    show("submit_transformasi")

    removeClass(selector = "#step4_transformasi", class = "active")
    removeClass(selector = "#step3_transformasi", class = "active")
    removeClass(selector = "#step2_transformasi", class = "active")
    addClass(selector = "#step1_transformasi", class = "active")
  })

}

shinyApp(ui, server)
