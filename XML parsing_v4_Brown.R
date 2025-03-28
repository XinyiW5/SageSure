library(xml2)
library(dplyr)
 # ------------------ Brown University -----------------
# Website: https://projects.propublica.org/nonprofits/organizations/50258809

# Load the XML document
url <- "https://pp-990-xml.s3.us-east-1.amazonaws.com/202401319349305030_public.xml?response-content-disposition=inline&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIA266MJEJYTM5WAG5Y%2F20250314%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20250314T014856Z&X-Amz-Expires=1800&X-Amz-SignedHeaders=host&X-Amz-Signature=aa716d09814d3a4cdba5dc177057ab7862faecc0b7131002601da8801625c40f"
xml_data <- read_xml(url)

# Extract namespaces
ns <- xml_ns(xml_data)

# --- Extract All <Form990PartVIISectionAGrp> Blocks as Rows ---
form990_nodes <- xml_find_all(xml_data, "//d1:Form990PartVIISectionAGrp", ns)

form990_info <- lapply(form990_nodes, function(node) {
  children <- xml_children(node)
  setNames(xml_text(children), xml_name(children))
})

# Convert extracted data into a DataFrame (each block as a row)
if (length(form990_info) > 0) {
  form990_df <- bind_rows(form990_info)
} else {
  form990_df <- data.frame()
}

# Ensure unique column names (if necessary)
colnames(form990_df) <- make.names(colnames(form990_df), unique = TRUE)

# --- Display the DataFrame ---
print(form990_df)

# export
write.xlsx(form990_df, "Brown_form990_Extract.xlsx", rowNames = FALSE)
