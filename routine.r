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

# Ajoutez les fichiers en pièce jointe (spécifiez le chemin de vos fichiers)
fichiers_a_envoyer <- c("wikipedia/wikipedia.csv")
for (fichier in fichiers_a_envoyer) {
  email <- add_attachment(email, file = fichier)
}

# Envoyer l'email
smtp <- smtp_send(
  email,
  from = "charles.hebert.osfi@outlook.com",
  to = "charles.hebert@osfi-bsif.gc.ca",
  subject = "Fichiers demandés",
  credentials = creds_envvar(
    user = "charles.hebert.osfi@outlook.com",
    pass_envvar = "email_secret",
    host = "smtp.exemple.com",
    port = 465,
    use_ssl = TRUE
  )
)
