#cbc
#wikipedia
#trends
#webmasters tools
#my business profile
#envoi courriel



library(blastula)

email <- compose_email(
  body = md("Bonjour,

Voici les fichiers demandés en pièce jointe.
  
Pour toute info, n'hésitez pas à communiquer avec moi,
Charles")
)

# Ajout des fichiers en pièce jointe
fichiers_a_envoyer <- c("wikipedia/wikipedia.csv")
for (fichier in fichiers_a_envoyer) {
  email <- add_attachment(email, file = fichier)
}

# Envoi du courriel
smtp <- smtp_send(
  email,
  from = "charles.hebert.osfi@outlook.com",
  to = "charles.hebert@osfi-bsif.gc.ca",
  subject = "Fichiers demandés",
  credentials = creds_envvar(
    user = "charles.hebert.osfi@outlook.com",
    pass_envvar = "email_secret",
    host = "smtp.office365.com",
    port = 587,
    use_ssl = TRUE
  )
)
