# Data -----

getData <- function() {
  
  Reach::downloadOSFdata(repository  = 'm5dt4',
                         filelist    = list('data/' = c('control_aligned.zip',
                                                        'control_rotated.zip',
                                                        'handview_aligned.zip',
                                                        'handview_rotated.zip',
                                                        'cursorjump_aligned.zip',
                                                        'cursorjump_rotated.zip',
                                                        'demographics.csv'
                         )  ),
                         folder      = 'data/',
                         overwrite   = TRUE,
                         unzip       = TRUE,
                         removezips  = TRUE)
  
}

getData()

# Participants -----

# gets the list of participants that were in a specific group / condition

groupParticipants <- function(group) {
  
  demo <- read.csv('data/demographics.csv', stringsAsFactors = F)
  
  participants <- demo$participant[which(demo$group == group)]
  
  return(participants)
}
