data.urls <- c("https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/49379525-bd51-4d27-a9fb-a04151561898/download/yem_admin_20171007.gdb.zip",
               "https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/f942fe94-c28a-4e4d-9bed-d241bdce345b/download/yem_admin_20171007-.xlsx",
               "https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/1574d33c-c002-4294-a7c4-89433728c9b3/download/yemen_admin_20171007_shape.zip")

data.file.names <- basename(data.urls)

download.files