# R/services/services_certificates.R
# This script contains functions related to the certificates which are used across different modules

# this function expects a db connection, user_id for the therapist and a vector with all the user_ids for the members for which certificates should be generated
# Then, the function will get the member and abo info and use it to generate a pdf based on the quarto template for each member
# After generating the pdfs, they will be merged and placed inside a subfolder in the folder Zertifikate with the current date and time as name
make_certificates <- function(con, therapist_user_id, members_user_ids, abo_ids) {
  therapist <- get_therapist_user_id(con, therapist_user_id)
  
  file_path_template <- here("template", "pdf-output.qmd")
  time <- str_replace(substring(now(), first = 12, last = 16), pattern = ":", replacement = "_")
  file_path_output <- here("Zertifikate", paste(format(today(), "%Y_%m_%d"), time, sep = "_"))

  dir.create(file_path_output, showWarnings = FALSE)
  
  withProgress(message = "Zertifikate werden erstellt...\n", value = 0, {
    n <- length(members_user_ids)

    for (i in seq_along(members_user_ids)) {
      member <- members_user_ids[i]
      member_data <- get_member_user_id(con, member)
      abo_data <- get_abo_abo_id(con, abo_ids[i])
      course_membership <- get_course_membership_user_id(con, member)$course_id
      abo_price <- get_abo_price(con, course_membership, abo_data$abo_type)$abo_price

      incProgress(1/n, detail = paste("Erstelle Zertifikat für", paste(member_data$vorname, member_data$name)))
      
      quarto_render(
        input = file_path_template,
        output_format = "pdf",
        output_file = paste0("Zert_", member_data$vorname, "_", member_data$name, ".pdf"),
        execute_params = list(
          pt_vorname = therapist$vorname,
          pt_name = therapist$name,
          pt_praxis = therapist$praxis,
          pt_adresse = therapist$adresse,
          pt_plz = therapist$plz,
          pt_tel = therapist$tel,
          pt_mail = therapist$mail,
          pt_zsr = therapist$zsr,
          pt_knr = therapist$knr,
          pt_emfit = therapist$emfit,
          pt_pilat_nr = therapist$pilat_nr,
          abo_type = abo_data$abo_type,
          abo_start = format_swiss_date_char(abo_data$abo_start),
          abo_end = format_swiss_date_char(abo_data$abo_end),
          abo_price = abo_price,
          mem_kk = member_data$kk,
          mem_zv = member_data$zv,
          mem_vnr = member_data$vnr,
          mem_name = member_data$name,
          mem_vorname = member_data$vorname,
          mem_adresse = member_data$adresse,
          mem_plz = member_data$plz,
          mem_mail = member_data$mail
        ),
        quarto_args = c("--output-dir", file_path_output) # pass output directory
      )
    }

    # merge all pdfs in folder
    if (n > 1) {
      files <- list.files(file_path_output, pattern = "\\.pdf$", full.names = TRUE)

      pdf_combine(
        input = files,
        output = paste(file_path_output, "alle_Zertifikate.pdf", sep = "/")
      )
    }
  })
}
