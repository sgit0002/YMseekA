provider_type_df[(provider_type_df$Sex == 'Female' & provider_type_df$Provider_type == 'All providers' ), ]
x = filter(provider_type_df, (Sex == 'Female'))
filter(x, (Provider_type == 'All providers'))
provider_type_df$Provider_type
