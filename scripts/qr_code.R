
library(qrcode)
library(here)

q <- qr_code("https://www.themusiclab.org/quizzes/toneguesser")

plot(q)
generate_svg(filename = here("toneguesser_QR.svg"), qrcode = q, background = "white", foreground = "#00467F")
