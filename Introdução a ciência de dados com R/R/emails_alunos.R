rm(list = ls())

library(gmailr)
library(htmltools)
library(htmlwidgets)
library(googlesheets4)
library(stringr)


# Header ------------------------------------------------------------------


### Login com uma conta google para acessar as planilhas do google sheets
## secrets é a pasta onde o R irá salvar o arquivo de credencias para não precisar mais pedir login
options(gargle_oauth_cache = "secrets")
gs4_auth(
  email = 'caiogcg.mobilidade@gmail.com'
)

### Lendo as respostas da pesquisa respondida em aula pela id da sheet

sheet <- read_sheet(ss = '1wtOKaVgt2mkvwPmlGLWeWEEQJG2DqWQkXjRLW1uqpiI')



# Email html --------------------------------------------------------------

## Pra essa parte você vai precisar entender a lógica html, mas o R te ajuda o pacote
## htmltools tem um suporte incrivel para gerar páginas web completas com html a partir do R

endcourse_email <- function(student_name){
  first_name <- strsplit(student_name,' ') %>% 
    unlist() %>% .[1] %>% str_to_title()
  
  tags$div(
    id = 'email-content',
    tags$head(
      tags$link(
        href = 'https://fonts.googleapis.com/css?family=Montserrat',
        rel = 'stylesheet'
      )
    ),
    style = 'background-color: #53777a3b;max-width:800px;font-family: Montserrat;',
    tags$div(
      style = 'background-color: #722E9C;width:100%;height:100px;display:flex;',
      tags$img(
        src = 'https://raw.githubusercontent.com/cgmobility/r-courses.public/refs/heads/main/Introdu%C3%A7%C3%A3o%20a%20ci%C3%AAncia%20de%20dados%20com%20R/RStudio.png',
        style = '
        height: 60px;
        margin-left: 20px;
        margin-top: auto;
        margin-bottom: auto;'
      ),
      tags$h2(
        'INTRODUÇÃO À CIÊNCIA DE DADOS COM R',
        style = 'text-align:center;color: white;margin:auto;'
      ),
      # tags$img(
      #   src = 'https://www.unichristus.edu.br/wp-content/uploads/2022/10/Logo-Unichristus-em-Branco.png',
      #   style = '
      #   height: 30px;
      #   margin-left: auto;
      #   margin-top: auto;
      #   margin-bottom: auto;
      #   margin-right: 20px;'
      # )
    ),
    tags$div(
      id = 'email-body',
      style = 'margin-left: 20px;margin-right: 20px; text-align: justify;',
      tags$h2(
        paste0('Olá ',first_name,'!'),
        style = 'margin-bottom:20px;text-align:center;'
      ),
      tags$h4(
        'Gostaríamos de agradecer pela sua participação no curso ciencia de dados com R! Esperamos que ele tenha ajudado você a destravar novas habilidades na linguagem e a explorar o potencial que o R oferece para análise de dados e visualização.'
      ),
      tags$h4(
        'O R é uma ferramenta poderosa e versátil, capaz de transformar grandes volumes de dados em informações claras e estratégicas. Nossa jornada até aqui foi só o começo, e esperamos que você se sinta mais preparado(a) para explorar essa linguagem incrível.'
      ),
      tags$h4(
        'Informamos que todo o material do curso estará permanentemente disponível no drive do curso'
      ),
      tags$div(
        tags$a(
          href = 'https://drive.google.com/drive/folders/12DD_dYiwJf9bkjvAwAZ2qNEIvMQAxFQW?usp=drive_link',
          target = '_blank',
          style = 'margin-left:auto;margin-right:auto;width:50%;
          background-color: #4b875d;display:flex;
          border-radius: 20px;padding: 5px;',
          tags$img(
            src = '//ssl.gstatic.com/images/branding/product/1x/drive_2020q4_48dp.png',
            style = 'height: 59px;margin-left: 5%;',
            tags$p(
              'Acesse o drive do curso!',
              style = 'color:white;margin:auto;font-size: large;font-weight: bold;'
            )
          )
        )
      ),
      tags$h4(
        'Para nós, seu feedback é essencial! Pedimos que você compartilhe suas impressões sobre o curso na pesquisa de avaliação abaixo. Suas respostas nos ajudam a continuar aprimorando o conteúdo e a experiência:'
      ),
      tags$div(
        tags$a(
          href = 'https://forms.gle/w36yBJJ4AddNcrjN8',
          target = '_blank',
          style = 'margin-left:auto;margin-right:auto;width:50%;
          background-color: #8e5eab;display:flex;
          border-radius: 20px;padding: 5px;',
          tags$img(
            src = 'https://cdn-icons-png.flaticon.com/512/5968/5968528.png',
            style = 'height: 59px;margin-left: 5%;',
            tags$p(
              'Queremos sua opinião!',
              style = 'color:white;margin:auto;font-size: large;font-weight: bold;'
            )
          )
        )
      ),
      tags$h4(
        'Além disso, não deixe de acompanhar nossos perfis nas redes sociais para mais dicas, novidades e, claro, para nos mantermos conectados:'
      ),
      tags$div(
        
        tags$div(
          style = 'display:flex;',
          tags$a(
            href = 'https://www.linkedin.com/in/gustavo-guimaraes-5b0132128/',
            target="_blank",
            style = 'display:flex;',
            tags$img(
              src="https://cdn-icons-png.flaticon.com/512/145/145807.png", 
              style="width:30px; height:30px; margin:5px; vertical-align:middle; margin-left:5px;",
              alt="Linkedin"
            )
          ),
          tags$a(
            href = 'https://www.instagram.com/gustav.guimaraes/',
            target="_blank",
            style = 'display:flex;',
            tags$img(
              src="https://cdn-icons-png.flaticon.com/512/1409/1409946.png ", 
              style="width:30px; height:30px; margin:5px; vertical-align:middle; margin-left:5px;",
              alt="Instagram"
            )
          ),
          tags$p(
            'Caio Guimarães',
            style = 'margin-left: 5px; color: black;margin-top:auto;'
          )
        ),
        tags$div(
          style = 'display:flex;',
          tags$a(
            href = 'https://www.linkedin.com/in/nelsonquesado/',
            target="_blank",
            style = 'display:flex;',
            tags$img(
              src="https://cdn-icons-png.flaticon.com/512/145/145807.png", 
              style="width:30px; height:30px; margin:5px; vertical-align:middle; margin-left:5px;",
              alt="Linkedin"
            )
          ),
          tags$a(
            href = 'https://www.instagram.com/nelsonquesado/',
            target="_blank",
            style = 'display:flex;',
            tags$img(
              src="https://cdn-icons-png.flaticon.com/512/1409/1409946.png ", 
              style="width:30px; height:30px; margin:5px; vertical-align:middle; margin-left:5px;",
              alt="Instagram"
            )
          ),
          tags$p(
            'Nelson Quesado',
            style = 'margin-left: 5px; color: black;margin-top:auto;'
          )
        )
        
      ),
      tags$h4(
        'E qualquer dúvida ou sugestão, estamos disponíveis nos emails abaixo:'
      ),
      tags$div(
        tags$a(
          href = 'mailto:caiogcg.mobilidade@gmail.com',
          target="_blank",
          style = 'display:flex;',
          tags$img(
            src="https://cdn-icons-png.flaticon.com/512/5968/5968534.png", 
            style="width:20px; height:20px; vertical-align:middle; margin-left:5px;",
            alt="Gmail"
          ),
          tags$p(
            'Caio Guimarães',
            style = 'margin-left: 5px; color: black;margin-top: auto;margin-bottom: auto;'
          )
        ),
        tags$a(
          href = 'mailto:nquesado@gmail.com',
          target="_blank",
          style = 'display:flex;',
          tags$img(
            src="https://cdn-icons-png.flaticon.com/512/5968/5968534.png", 
            style="width:20px; height:20px; vertical-align:middle; margin-left:5px;",
            alt="Gmail"
          ),
          tags$p(
            'Nelson Quesado',
            style = 'margin-left: 5px; color: black;margin-top: auto;margin-bottom: auto;'
          )
        )
        
      ),
      tags$h4(
        'Muito obrigado mais uma vez, e esperamos que o R continue sendo uma ferramenta valiosa para sua jornada. Sucesso e nos vemos em breve!'
      ),
      tags$h4(
        'Um grande abraço!',
        tags$br(),
        'Caio Gustavo Guimarães e Nelson Quesado'
      ),
      tags$div(
        ' ',
        style = 'height:20px;'
      ),
      tags$h5(
        style = 'text-align:center;',
        'Este email foi gerado e enviado utilizando o R.',
        tags$a(
          href = 'https://github.com/cgmobility/r-courses.public/blob/main/Introdu%C3%A7%C3%A3o%20a%20ci%C3%AAncia%20de%20dados%20com%20R/R/emails_alunos.R',
          target = '_blank',
          'Veja como.'
        )
      )
    )
  ) %>% return()
}




# Envio usando gmailR -----------------------------------------------------


path_old <- paste0(getwd(),'/secrets/client_secret_987464996782-sjcsbd0erhmcf986tokhppple499p929.apps.googleusercontent.com.json')
fs::file_copy(path_old, fs::dir_create(rappdirs::user_data_dir("gmailr"), recurse = TRUE))


Sys.setenv(GMAILR_OAUTH_CLIENT=paste0(getwd(),'/secrets/client_secret_987464996782-sjcsbd0erhmcf986tokhppple499p929.apps.googleusercontent.com.json'))

gm_auth_configure()

gm_oauth_client()

for (i in 2:nrow(sheet)) {
  
  email_dest <- sheet$`Endereço de e-mail`[i]
  student_nm <- sheet$`Nome completo`[i]
  
  mail_content <- gm_mime() %>% 
    gm_to(c(email_dest,'nquesado@gmail.com')) %>% 
    gm_from('caiogcg.mobilidade@gmail.com') %>% 
    gm_subject('Obrigado por explorar o mundo do R com a gente!') %>% 
    gm_html_body(as.character(endcourse_email(student_nm)))
  
  gm_send_message(mail_content)
  
}



