library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(magrittr)

read_csv2("cim10.csv") %>%
  mutate(famille_libelle = famille_libelle %>% str_replace("\\w\\d+ ", ""),
         CMD_libelle = CMD_libelle %>% str_replace("\\d+ ", "")) %>%
left_join(read_csv("CMA_Gyn.txt", col_names = "code") %>%
            mutate(gyn = "O"),
          by = c("diag_code" = "code")) -> cim10

# groupes ----------
read_csv2("groupes.csv") %>%
  separate(groupe_code, sep = "-", into = c("borne_inf", "borne_sup"), remove = F) %>%
  unite(groupe, groupe_code, groupe_libelle, sep = " ", remove = F) -> groupes

cim10 %<>%
  mutate(groupe = famille_code %>%
           map_chr(. %>%
           {
             ifelse(identical(groupes$groupe[. >= groupes$borne_inf & . <= groupes$borne_sup], character(0)),
                    "",
                    groupes$groupe[. >= groupes$borne_inf & . <= groupes$borne_sup] %>%
                      str_c(collapse = "|"))
           })) %>%
  unite(chapitre, chapitre_code, chapitre_libelle, sep = " ") %>%
  unite(famille, famille_code, famille_libelle, sep = " ") %>%
  unite(diag, diag_code, diag_libelle, sep = " ") %>%
  unite(CMD, CMD_code, CMD_libelle, sep = " ") %>%
  mutate(famille = ifelse(famille == diag, "", famille)) %>%
  mutate(path = str_c(chapitre, groupe, famille, diag, sep = "|") %>%
                str_replace("\\|{2,}", "|") %>%
                str_split("\\|")) %>%
  select(path, CMA, gyn) %>%
  filter(!is.na(CMA), CMA > 1 | gyn == "O")

rm(groupes)

cim10 %<>% tree
cim10 %<>% summ_var("CMA")
cim10 %<>% summ_var("gyn")
cim10 %>% tree2html -> cim10_html

tree <- function(df)
{
  if ((df$path %>% map_dbl(length) == 0) %>% all)
  {
    df %>% select(-path)
  } else
  {
    df$path %>% map(head, 1) -> df$nodes
    df$path %<>% map(tail, -1)

    df$nodes %>% flatten_chr %>% unique %>%
      sapply(simplify = F, function(node)
             {
               df %>% filter(nodes == node) %>% select(-nodes)
             }) %>%
      sapply(simplify = F, tree)
  }
}

add_names <- function(tr, name = NULL)
{
  if (tr %>% is.data.frame)
  {
    tr$name <- name
    tr
  } else if (tr %>% is.atomic)
  {
    tr
  } else
  {
    tr %>% map2(tr %>% names, add_names)
  }
}

untree <- function(tr)
{
  empty_path <- function(tr)
  {
    if (tr %>% is.data.frame)
    {
      tr$path <- rep(list(character(0)), nrow(tr))
      tr
    } else
    {
      tr %>% map(empty_path)
    }
  }

  tr %<>% empty_path

  untree_ <- function(tr)
  {
    if (tr %>% map_lgl(is.data.frame) %>% all)
    {
      tr %>% names %>% rep(tr %>% map_dbl(nrow)) -> nodes
      tr %>%
        reduce(bind_rows) %>%
        mutate(path = map2(nodes, path, splice))
    } else if (tr %>% is.data.frame)
    {
      tr
    } else
    {
      tr %>% map(untree_)
    }
  }

  while (!is.data.frame(tr))
  {
    tr %<>% untree_
  }

  tr
}

summ_var <- function(tr, varname = NULL)
{
  if (varname %>% is.null)
  {
    return(tr)
  }

  if (tr %>% is.data.frame | tr %>% is.atomic)
  {
    tr
  } else
  {
    tr %<>% map(summ_var, varname)
    tr[[varname]] <- tr %>% map(varname) %>% unlist %>% unique %>% sort
    tr
  }
}

tree2html <- function(tr)
{
  if (!is.data.frame(tr))
  {
    tr %>% Filter(is.list, .) %>% names -> names
    str_c('<ul>\n',
          tr %>% Filter(is.list, .) %>% map2(names, function(tr, name)
          {
            labels <- c("info", "success", "warning", "danger")
            label <- labels[tr$CMA]
            label2 <- ifelse(length(tr$gyn) == 1, '<span class = "label label-info">O</span>', '')
            str_c("<li>", name, str_c('<span class = "label label-', label, '">', tr$CMA, "</span>", collapse = " "), label2, tree2html(tr), "</li>", sep = " ")
          }) %>% str_c(collapse = "\n"),
          "</ul>")
  }
}

df <- data.frame(path = c(str_c("A", "A1", "A11", sep = "|"),
                          str_c("A", "A1", "A11", sep = "|"),
                          str_c("A", "A1", "A12", sep = "|"),
                          str_c("A", "A2", sep = "|"),
                          str_c("B", sep = "|")),
                 var1 = letters[1:5],
                 var2 = 1:5,
                 stringsAsFactors = F) %>%
      mutate(path = path %>% str_split("\\|"))
