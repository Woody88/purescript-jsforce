module PSObject.Standard where
  
data UserInfo 
    = UserInfo { id             :: String 
               , organizationId :: String 
               , url            :: String 
               }