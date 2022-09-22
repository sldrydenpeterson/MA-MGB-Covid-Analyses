
# Table 1 formation, drawn from this site: https://michaeldismorr.netlify.app/post/publication-ready-tables-with-flextable-and-own-theme-in-r/
tab1_defaults <- function(){
  set_flextable_defaults(font.family = "Calibri", 
                         font.size = 10, 
                         border.color = "black")
}
custom_tab <- function(df, header, footer){
  flextable(df) %>% 
    add_header_lines(header) %>% 
    add_footer_lines(footer) %>% 
    bold(i = 1, part = "header") %>% 
    hline_top(part = "header", 
              border = fp_border(color = "red", 
                                 width = 3, 
                                 style = "solid")) %>% 
    hline(i = 1, 
          part = "header", 
          border = fp_border(color = "black", 
                             width = 0.25, 
                             style = "solid")) %>% 
    hline_top(part = "body", 
              border = fp_border(color = "black", 
                                 width = 0.25, 
                                 style = "solid")) %>% 
    hline_bottom(part = "body", 
                 border = fp_border(color = "black", 
                                    width = 0.25, 
                                    style = "solid")) %>% 
    hline_bottom(part = "footer", 
                 border = fp_border(color = "black", 
                                    width = 0.25, 
                                    style = "solid")) %>% 
    border_inner_h(part = "body", 
                   border = fp_border(color = "black", 
                                      width = 0.25, 
                                      style = "dotted")) %>% 
    autofit(part = "body") %>% 
    bg(part = "body", bg = "#f5f5f5") %>% 
    align(part = "all", align = "center") %>% 
    align(j = 1, part = "all", align = "left")
}