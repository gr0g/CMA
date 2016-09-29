var treeInit = function(root, searchbox)
{
  $(root).addClass("tree");
  // Attribut value de chaque li contient l'ensemble du texte de ses descendants (chemin d'accès aux résultats de recherche facile)
  $(root).find('li').
    map(function()
        {
          $(this).
            attr("value",
                 $(this).
                   text().
                   replace(/^ *\n/mg, "").
                   replace(/ +/g, " ").
                   replace(/( \d)*/g, "").
                   replace(/ O /g, " ").
                   toLowerCase())
        });

  // Tous les ul sauf le premier son cachés, et leurs parents li prennent l'attribut closed
  $(root).
    find('li').
      children('ul').hide().
      parent().addClass("closed");

  // Évènements
  // Cliquer pour ouvrir/fermer un noeud
  $(root).find('li').click(toggleNode);
  // Chercher dans l'arbre en tapant dans le champ de recherche
  $(searchbox).keyup($(root), searchTree);
  // Recherche par niveau de cma au clic sur un niveau de cma
  $(root).find('li > span').click($(searchbox), searchCMA);
}

var toggleNode = function(event)
{
    var target = $(event.target);

    if (target.hasClass("closed"))
    {
      // Deplier explicitement fait toujours tout afficher
      target.
        children('ul').slideDown().
        find('li').slideDown();
      // Et switch l'attribut du li
      target.
        removeClass("closed").
        addClass("open");
    }
    else if (target.hasClass("open"))
    {
      // Replier explicitement ne fait que cacher l'ul
      target.
        children('ul').slideUp();
      // Et switch l'attribut du li
      target.
        removeClass("open").
        addClass("closed");
    }
    event.stopPropagation();
}

var searchTree = function(event)
{
    var tree = event.data;
    var val = $(event.target).val().toLowerCase();

    // à chaque actualisation de la recherche ferme tout et rétablit l'attribut closed
    $(tree).find('li').
      children('ul').hide().
      parent().addClass("closed").
               removeClass("open");

    // si le champ n'est pas assez rempli on remontre bien tous les noeuds
    if (val.length < 1)
    {
      $(tree).find('li').show();
      return;
    }

    var result = $(tree).find('li[value *= "'+ val +'"]');

    // on cache tous les noeuds puis on montre et déplie les résultats
    $(tree).find('li').hide();
    result.show().
           children('ul').show();

    // si tous les sous-noeuds sont montrés par la recherche, on met le statut du parent à open
    result.children('ul').each(function()
    {
      if ($(this).children('li[style *= "none"]').length == 0)
      {
        $(this).parent().addClass("open").removeClass("closed");
      }
    });
}

var searchCMA = function(event)
{
    var searchbox = event.data;
    var this_ = $(event.target).parent();
    var cma = this_.add(this_.find('li > span:contains("' + $(this).text() + '")').parent());

    // cacher tous les sous-noeuds
    this_.find('li').hide();

    // capture de résultats de la recherche sous le noeud pour continuer à les afficher
    if (typeof $(searchbox).val() != 'undefined')
    {
      var result = this_.find('li[value *= "'+ $(searchbox).val().toLowerCase() +'"]');

      result.show().
        children('ul').show();
    }

    // this_ est-il une feuille ?
    var leaf = !(this_.hasClass("closed") || this_.hasClass("open"))

    this_.show().
          addClass("closed").
          removeClass("open").
      children('ul').show();

    if (leaf)
      this_.removeClass("closed");

    cma.show().
           children('ul').show();

    cma.children('ul').each(function()
    {
      if ($(this).children('li[style *= "none"]').length == 0)
      {
        $(this).parent().addClass("open").removeClass("closed");
      }
    });
  }
