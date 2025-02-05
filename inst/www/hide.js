
Shiny.addCustomMessageHandler("toggle_side", function(data){

if(data) {
  $("#side_bar").css("display","none")
    // $("#help_nonclinical_02").css("display","block")
    // $("#help_button_space").css("padding-bottom", "15px")
} else{
  // $("#side_bar").hide()
  $("#side_bar").css("display","block")
    // $("#help_nonclinical_02").css("display","none")
    // $("#help_button_space").css("padding-bottom", "1px")
}
}
)
