# function to set the language argument in the options for datatables
german_datatable <- function() {
  list(
    search = "Suche:",
    lengthMenu = "_MENU_ Einträge anzeigen",
    info = "_START_ bis _END_ von _TOTAL_ Einträgen",
    infoEmpty = "0 Einträge",
    paginate = list(
      first = "Erste",
      last = "Letzte",
      `next` = "Nächste",
      previous = "Vorherige"
    ),
    emptyTable = "Keine Daten vorhanden",
    zeroRecords = "Keine Einträge gefunden"
  )
}