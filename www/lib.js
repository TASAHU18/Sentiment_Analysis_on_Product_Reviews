setInterval(function(){
  if ($('html').hasClass('shiny-busy')==true) {
    $('#loading_indicator').show()
	
  } else {
    $('#loading_indicator').hide()
  }
},1000)