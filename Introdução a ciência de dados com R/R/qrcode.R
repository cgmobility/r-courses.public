library(qrcode)

LDA::wd()

# Pesquisa de email -------------------------------------------------------

qrcode <- qr_code('https://forms.gle/uey1pAmjMobCNumB8',ecl = 'H')

qrcode <- add_logo(
  qrcode,
  logo = '../R - RStudio_logo_flat.svg.png'
)

generate_svg(qrcode,'qrcode_basic_survey.svg',size = 500)



# Drive do curso ----------------------------------------------------------

qrcode <- qr_code('https://drive.google.com/drive/folders/12DD_dYiwJf9bkjvAwAZ2qNEIvMQAxFQW?usp=sharing',ecl = 'H')

qrcode <- add_logo(
  qrcode,
  logo = '../R - RStudio_logo_flat.svg.png'
)

generate_svg(qrcode,'qrcode_student_drive.svg',size = 500)




