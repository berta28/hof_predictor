library(shiny)
# shiny::runGitHub("berta28/hof_predictor", ref = "main")

rsconnect::setAccountInfo(name='07t44a-sean-berta',
			  token='2B04B0E0B3F06761281FBB1C59D698D9',
			  secret='MHdpNKngrsJdkBIEID+fO1gUidMxWshB0MMoI0GL')
# terminateApp("hof_predictor")
rsconnect::showLogs()
rsconnect::deployApp(getwd())