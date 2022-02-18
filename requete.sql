--le joueur le plus jeune contenu dans la base de donnees
select nom_joueur, prenom_joueur
  from joueur where datenaissance =
                             (select max(datenaissance) from joueur);
--le Jouer le plus grand contenu dans la base de données
select nom_joueur, prenom_joueur
  from joueur where datenaissance =
                             (select MIN(datenaissance) from joueur);

-- les buts marqués pour le WAC à domicile et le RAJAH à l'exterieur
SELECT SUM(scoreLocale),SUM(scoreVisiteur)
FROM games WHERE equipeLocale = '1111' and equipeVisiteur='1010'

-- le nom des jouers qui sont nés entre 1980 et 1990
SELECT nom_joueur,prenom_joueur FROM joueur WHERE dateNaissance BETWEEN '1980-01-01 00:00:00' AND  '1990-01-01 00:00:00'
-- les noms des jouers qui commencent par la lette A
Select *
From joueur
Where nom_joueur like 'A%'
-- le nom des jouers qui pésent 80 Kg
SELECT nom_joueur,prenom_joueur
FROM joueur
where poids=70;
-- l'equipe dans lesquelle le plus jeune joueurs a jouet et la ligue associee
select nom_joueur, prenom_joueur, nom_equipe, nom_ligue
from joueur, engager, equipe, ligue
where joueur.joueurid = engager.joueurid
and equipe.equipeid = engager.equipeid
and equipe.ligueid = ligue.ligueid
and datenaissance =
(select max(datenaissance) from joueur)

--le joueur le plus jeune de la saison 2008/2009
select nom_joueur, prenom_joueur
from joueur, engager, saison
where joueur.joueurid = engager.joueurid
  and engager.saisonid = saison.saisonid
  and saison.libelle = '2008/2009'
  and datenaissance =
    (select max(datenaissance) from joueur, engager, saison
      where joueur.joueurid = engager.joueurid
        and engager.saisonid = saison.saisonid
        and saison.libelle = '2008/2009');
-- Affichez le joueur le plus jeune de chaque saison contenue dans la base de donn ́ees. Pourchacun, vous afficherez son nom et l’ˆage qu’il avait au d ́ebut de la saison pendant laquelleil  ́etait le plus jeune.
select nom_joueur, prenom_joueur, saison.libelle
from joueur, engager, saison
where joueur.joueurid = engager.joueurid
  and engager.saisonid = saison.saisonid
  and datedeb-datenaissance =
    (select min(datedeb-datenaissance) from joueur, engager, saison s2
      where joueur.joueurid = engager.joueurid
        and engager.saisonid = s2.saisonid
        and s2.saisonid = saison.saisonid
    );
--Affichez le nombre total de buts pour chaque ligue lors de la saison 2015/2016
select nom_ligue, sum(scorelocale + scorevisiteur)
from games, equipe, ligue, saison
where games.dategames between saison.datedeb and saison.datefin
 and games.equipelocale = equipe.equipeid
 and equipe.ligueid = ligue.ligueid
 and saison.libelle = '2015/2016'
group by ligue.ligueid, ligue.nom_ligue
order by sum(scorelocale + scorevisiteur) desc

--Le nombre de but marqué pour chaque équipe pendant les 11 saison.
SELECT equipe.nom_equipe, count(*)
from games, equipe, ligue,saison
where games.dategames between saison.datedeb and saison.datefin
 and games.equipelocale = equipe.equipeid
 and equipe.ligueid = ligue.ligueid
 group by equipe.equipeid, equipe.nom_equipe
