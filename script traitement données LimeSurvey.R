######Préparation traitement des données LimeSurvey####################

# --- Packages ---

library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)


# --- Chargement du fichier CSV ---
data <- read.csv("results-survey685762.csv", sep = ";", quote = "\"", header = TRUE, stringsAsFactors = FALSE)

# --- Nettoyage des données ---

#On renomme les colonnes

rename_table <- data.frame(
  old = c("Genre", "Ecole.Université.d.origine", "Domaine.d.étude.Discipline","Êtes.vous.boursier.e.", "Si.vous.êtes.boursier.e..à.quel.échelon..","Sur.le.plan.politique..diriez.vous.que.vous.vous.situez.plutôt...........",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Epargner..mettre.de.côté.pour.des.projets..",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Voyager.vers.des.régions.lointaines.en.avion..",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Acheter.du.matériel.de.musique.ou.sportif..et.ou.faire.des.sorties.culturelles..dépenses.loisirs.passions..hors.voyages...",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Acheter.des.nouveaux.appareils.électroniques..PC..smartphone......",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Donner.à.des.associations.ou.oeuvres.caritatives..",
          "Mise.en.situation..Imaginez.que.vous.gagniez.10000.euros..Comment.les.dépenseriez.vous...Indiquez.la.part.de.vos.dépenses.allouées.à.chacune.des.activités.suivantes...NB..Les.réponses.étant.en.pourcentage..leur.somme.doit.être.égale.à.100.......Autre..Préciser.ci.dessous..",
          "Mise.en.situation..Imaginez.que.vous.faites.les.courses.et.que.vous.avez.le.choix.entre.un.panier.de.fruits.qui.coûte.dix.euros.en.version..classique...non.bio..et.un.panier..bio..qui.coûte.plus.cher..À.quel.prix.choisiriez.vous.le.panier.de.fruits.bio.plutôt.que.sa.version.classique...Entourer.si.vous.êtes.prêt.e..ou.non.à.acheter.le.panier.aux.différents.prix.suggérés......Je.choisirais.la.version.bio.si.son.prix.était......NB..Si.vous.êtes.complètement.indifférent.e.à.la.caractéristique..bio...merci.de.répondre..non..à.chaque.ligne.du.tableau....Le.même.prix.que.la.version.classique.",
          "Mise.en.situation..Imaginez.que.vous.faites.les.courses.et.que.vous.avez.le.choix.entre.un.panier.de.fruits.qui.coûte.dix.euros.en.version..classique...non.bio..et.un.panier..bio..qui.coûte.plus.cher..À.quel.prix.choisiriez.vous.le.panier.de.fruits.bio.plutôt.que.sa.version.classique...Entourer.si.vous.êtes.prêt.e..ou.non.à.acheter.le.panier.aux.différents.prix.suggérés......Je.choisirais.la.version.bio.si.son.prix.était......NB..Si.vous.êtes.complètement.indifférent.e.à.la.caractéristique..bio...merci.de.répondre..non..à.chaque.ligne.du.tableau....Jusqu.à.10.50....max..5..plus.cher..",
          "Mise.en.situation..Imaginez.que.vous.faites.les.courses.et.que.vous.avez.le.choix.entre.un.panier.de.fruits.qui.coûte.dix.euros.en.version..classique...non.bio..et.un.panier..bio..qui.coûte.plus.cher..À.quel.prix.choisiriez.vous.le.panier.de.fruits.bio.plutôt.que.sa.version.classique...Entourer.si.vous.êtes.prêt.e..ou.non.à.acheter.le.panier.aux.différents.prix.suggérés......Je.choisirais.la.version.bio.si.son.prix.était......NB..Si.vous.êtes.complètement.indifférent.e.à.la.caractéristique..bio...merci.de.répondre..non..à.chaque.ligne.du.tableau....Jusqu.à.11....max..10..plus.cher..",
          "Mise.en.situation..Imaginez.que.vous.faites.les.courses.et.que.vous.avez.le.choix.entre.un.panier.de.fruits.qui.coûte.dix.euros.en.version..classique...non.bio..et.un.panier..bio..qui.coûte.plus.cher..À.quel.prix.choisiriez.vous.le.panier.de.fruits.bio.plutôt.que.sa.version.classique...Entourer.si.vous.êtes.prêt.e..ou.non.à.acheter.le.panier.aux.différents.prix.suggérés......Je.choisirais.la.version.bio.si.son.prix.était......NB..Si.vous.êtes.complètement.indifférent.e.à.la.caractéristique..bio...merci.de.répondre..non..à.chaque.ligne.du.tableau....Jusqu.à.12.....max.20..plus.cher..",
          "Mise.en.situation..Imaginez.que.vous.faites.les.courses.et.que.vous.avez.le.choix.entre.un.panier.de.fruits.qui.coûte.dix.euros.en.version..classique...non.bio..et.un.panier..bio..qui.coûte.plus.cher..À.quel.prix.choisiriez.vous.le.panier.de.fruits.bio.plutôt.que.sa.version.classique...Entourer.si.vous.êtes.prêt.e..ou.non.à.acheter.le.panier.aux.différents.prix.suggérés......Je.choisirais.la.version.bio.si.son.prix.était......NB..Si.vous.êtes.complètement.indifférent.e.à.la.caractéristique..bio...merci.de.répondre..non..à.chaque.ligne.du.tableau....Encore.plus.cher.que.cela..plus.de.20..plus.cher..",
          "Mise.en.situation..Imaginez.que.vous.soyez.au.restaurant.et.que.vous.choisissiez.un.menu..dans.lequel.vous.avez.le.choix.entre.deux.plats..au.même.prix.donc...un.plat.bio.à.base.de.poulet..et.un.plat.non.bio.à.base.de.lentilles..Laquelle.de.ces.deux.options.vous.semble.préférable.suivant.les.trois.critères.suivants.....Ma.préférence.en.termes.de.goût.",
          "Mise.en.situation..Imaginez.que.vous.soyez.au.restaurant.et.que.vous.choisissiez.un.menu..dans.lequel.vous.avez.le.choix.entre.deux.plats..au.même.prix.donc...un.plat.bio.à.base.de.poulet..et.un.plat.non.bio.à.base.de.lentilles..Laquelle.de.ces.deux.options.vous.semble.préférable.suivant.les.trois.critères.suivants.....Le.choix.le.plus.bénéfique.pour.la.planète..moindre.pollution..etc..",
          "Mise.en.situation..Imaginez.que.vous.soyez.au.restaurant.et.que.vous.choisissiez.un.menu..dans.lequel.vous.avez.le.choix.entre.deux.plats..au.même.prix.donc...un.plat.bio.à.base.de.poulet..et.un.plat.non.bio.à.base.de.lentilles..Laquelle.de.ces.deux.options.vous.semble.préférable.suivant.les.trois.critères.suivants.....Le.choix.le.plus.bénéfique.pour.ma.santé.",
          "Mise.en.situation..Imaginez.que.vous.soyez.au.restaurant.et.que.vous.choisissiez.un.menu..dans.lequel.vous.avez.le.choix.entre.deux.plats..au.même.prix.donc...un.plat.bio.à.base.de.poulet..et.un.plat.non.bio.à.base.de.lentilles..Laquelle.de.ces.deux.options.vous.semble.préférable.suivant.les.trois.critères.suivants.....Mon.choix.final..en.fonction.de.ces.3.critères.et.potentiellement.d.autres..",
          "Merci.de.confirmer.que.vous.avez.bien.lu.cette.introduction.",
          "D.après.vous..quel.pourcentage.....des.étudiant.e.s.participant.à.cette.étude.adoptent.ces.différents.comportements.au.moins.occasionnellement.......NB..Cette.question.vous.permet.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses....Poster.des.photos.stories.de.ses.loisirs.voyages.sur.les.réseaux.sociaux.",
          "D.après.vous..quel.pourcentage.....des.étudiant.e.s.participant.à.cette.étude.adoptent.ces.différents.comportements.au.moins.occasionnellement.......NB..Cette.question.vous.permet.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses....Acheter.des.vêtements.accessoires.appareils.en.seconde.main..Vinted..friperie..Fairphone....",
          "D.après.vous..quel.pourcentage.....des.étudiant.e.s.participant.à.cette.étude.adoptent.ces.différents.comportements.au.moins.occasionnellement.......NB..Cette.question.vous.permet.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses....Prendre.l.avion.pour.partir.en.vacances..",
          "D.après.vous..quel.pourcentage.....des.étudiant.e.s.participant.à.cette.étude.adoptent.ces.différents.comportements.au.moins.occasionnellement.......NB..Cette.question.vous.permet.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses....Boire.de.l.alcool..",
          "D.après.vous..quel.pourcentage.....des.étudiant.e.s.participant.à.cette.étude.adoptent.ces.différents.comportements.au.moins.occasionnellement.......NB..Cette.question.vous.permet.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses....Manger.de.la.viande.ou.du.poisson.",
          "Pour.les.deux.comportements.suivants..indiquez.ce.que.vous.pensez.être.la.fréquence.de.consommation.moyenne..en.nombre.de.jours.par.semaine..peu.importe.les.quantités..parmi.les.participant.e.s.adoptant.cette.pratique...NB..les.nombres.décimaux.sont.acceptés..Cette.question.vous.permet.à.nouveau.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses..et.que.vous.n.avez.pas.déjà.obtenu.cette.récompense.via.une.autre.question.de.ce.type.....Consommation.d.alcool.",
          "Pour.les.deux.comportements.suivants..indiquez.ce.que.vous.pensez.être.la.fréquence.de.consommation.moyenne..en.nombre.de.jours.par.semaine..peu.importe.les.quantités..parmi.les.participant.e.s.adoptant.cette.pratique...NB..les.nombres.décimaux.sont.acceptés..Cette.question.vous.permet.à.nouveau.de.gagner.5.euros.supplémentaires.si.vous.êtes.parmi.les.1..de.meilleures.réponses..et.que.vous.n.avez.pas.déjà.obtenu.cette.récompense.via.une.autre.question.de.ce.type.....Consommation.de.viande.ou.de.poisson.",
          "Poster.des.photos.stories.de.ses.loisirs.voyages.sur.les.réseaux.sociaux",
          "Prendre.l.avion.pour.partir.en.vacances",
          "Acheter.des.vêtements.accessoires.appareils.en.seconde.main..Vinted..friperie..reconditionné...",
          "Boire.de.l.alcool",
          "Manger.de.la.viande.ou.du.poisson",
          "Pour.le.comportement.suivant..merci.d.indiquer.à.quelle.fréquence.en.moyenne.vous.le.pratiquez..en.nombre.de.jours.par.semaine....Boire.de.l.alcool",
          "Pour.le.comportement.suivant..merci.d.indiquer.à.quelle.fréquence.vous.le.pratiquez..en.nombre.de.repas.par.semaine....Manger.de.la.viande......",
          "Pour.le.comportement.suivant..merci.d.indiquer.à.quelle.fréquence.vous.le.pratiquez..en.nombre.de.vols.par.an..un.aller.retour.compte.pour.deux.vols......Prendre.l.avion.pour.partir.en.vacances.....NB..Pour.toutes.les.questions.de.fréquence.dans.cette.fenêtre..les.nombres.décimaux.sont.autorisés..",
           "Cette.question.est.là.pour.vérifier.votre.attention..Merci.de.cocher.uniquement.la.réponse..Beaucoup..ci.dessous.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.poster..ou.non..sur.les.réseaux.sociaux.......Plaisir.personnel.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.poster..ou.non..sur.les.réseaux.sociaux.......Valeurs.personnelles.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.poster..ou.non..sur.les.réseaux.sociaux.......Envie.d.être.vu.e.comme.responsable.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.poster..ou.non..sur.les.réseaux.sociaux.......Envie.de.sociabiliser.et.ou.de.s.intégrer.",
          "Selon.vous..dans.quelle.mesure.ce.comportement.est.il.socialement.approprié..",
          "Selon.vous..quelle.proportion..en....des.autres.étudiant.e.s.participant.e.s.trouvent.ce.comportement.très.approprié.socialement.......NB..Une.bonne.réponse.à.cette.question.peut.vous.rapporter.cinq.euros.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.prendre.l.avion..ou.non..pour.partir.en.vacances....Plaisir.personnel.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.prendre.l.avion..ou.non..pour.partir.en.vacances....Valeurs.personnelles.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.prendre.l.avion..ou.non..pour.partir.en.vacances....Envie.d.être.vu.e.comme.responsable.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.prendre.l.avion..ou.non..pour.partir.en.vacances....Envie.de.sociabiliser.et.ou.de.s.intégrer.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.prendre.l.avion..ou.non..pour.partir.en.vacances....Prix..coût.monétaire..relativement.au.train..par.exemple..",
          "Selon.vous..dans.quelle.mesure.ce.comportement.est.il.socialement.approprié...1",
          "Selon.vous..quelle.proportion..en....des.autres.étudiant.e.s.participant.e.s.trouvent.ce.comportement.très.approprié.socialement....NB..Une.bonne.réponse.à.cette.question.peut.vous.rapporter.cinq.euros.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.d.acheter..ou.non..en.seconde.main....Goût.Plaisir.personnel.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.d.acheter..ou.non..en.seconde.main....Valeurs.personnelles.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.d.acheter..ou.non..en.seconde.main....Envie.d.être.vu.e.comme.responsable.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.d.acheter..ou.non..en.seconde.main....Envie.de.sociabiliser.et.ou.de.s.intégrer.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.d.acheter..ou.non..en.seconde.main....Prix..coût.monétaire..relativement.au.choix.alternatif.d.acheter.neuf..",
          "Selon.vous..dans.quelle.mesure.ce.comportement.est.il.socialement.approprié...2",
          "Selon.vous..quelle.proportion..en....des.autres.étudiant.e.s.participant.e.s.trouvent.ce.comportement.très.approprié.socialement....NB..Une.bonne.réponse.à.cette.question.peut.vous.rapporter.cinq.euros..1",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Goût.Plaisir.personnel.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Santé.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Valeurs.personnelles.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Envie.d.être.vu.e.comme.responsable.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Envie.de.sociabiliser.et.ou.de.s.intégrer.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.l.alcool....Prix..coût.financier.",
          "Selon.vous..dans.quelle.mesure.consommer.de.l.alcool.tous.les.jours.est.il.socialement.approprié..",
          "Selon.vous..quelle.proportion..en....des.autres.étudiant.e.s.participant.e.s.trouve.la.consommation.quotidienne.d.alcool.très.appropriée.socialement....NB..Une.bonne.réponse.à.cette.question.peut.vous.rapporter.cinq.euros.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Goût.Plaisir.personnel.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Santé.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Valeurs.personnelles.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Envie.d.être.vu.e.comme.responsable.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Envie.de.sociabiliser.et.ou.de.s.intégrer.",
          "Évaluez.l.importance.de.chacun.des.motifs.suivants.dans.la.décision.de.consommer..ou.non..de.la.viande....Prix.Coût.financier.",
          "Selon.vous..dans.quelle.mesure.consommer.de.la.viande.tous.les.jours.est.il.socialement.approprié..",
          "Selon.vous..quelle.proportion..en....des.autres.étudiant.e.s.participant.e.s.trouvent.la.consommation.quotidienne.de.viande.très.appropriée.socialement.......NB..Une.bonne.réponse.à.cette.question.peut.vous.rapporter.cinq.euros.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.8.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.8.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Vendredi.9.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Vendredi.9.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Lundi.12.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Lundi.12.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Mardi.13.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Mardi.13.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Mercredi.14.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Mercredi.14.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.15.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.15.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Vendredi.16.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Vendredi.16.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Lundi.19.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Lundi.19.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Mardi.20.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Mardi.20.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Mercredi.21.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Mercredi.21.01.à.12h50.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.22.01.à.12h.",
          "Seriez.vous.disponibles.pour.venir.manger......Jeudi.22.01.à.12h50.",
          "Temps.total..",
          "code.participant"),
  new = c("Gender", "School", "Main_discipline", "Scholarship_dummy","Scholarship_level","Political_proximity","Savings_share","Plane_share","Leisure_share","Matgoods_share","Donation_share", "Otherexp_share",
          "Organic_sameprice","Organic_5percmoreexp","Organic_10percmoreexp","Organic_20percmoreexp","Organic_evenmoreexp","Fav_hypdish_taste","Fav_hypdish_environment","Fav_hypdish_health","Chosen_hypdish","Attcheck1",
          "Percnorm_socials","Percnorm_sechand","Percnorm_plane","Percnorm_alcohol","Percnorm_meat","Percnorm_frequency_alcohol","Percnorm_frequency_meat",
          "Posting_socials_dummy","Flying_dummy","Buying_sechand_dummy","Alchoholdrinking_dummy","Meateating_dummy","Nbdays_alcoholdrinking_perweek","Nb_meatdishes_perweek","Nb_flights_peryear","Attcheck2",
          "Socials_hedonicmotive","Socials_valuemotive","Socials_imagemotive","Socials_integrationmotive", "Socialapprop_socials","Perc_injnorm_socials",
          "Plane_hedonicmotive","Plane_valuemotive","Plane_imagemotive","Plane_integrationmotive", "Plane_pricemotive", "Socialapprop_plane","Perc_injnorm_plane",
          "Sechand_hedonicmotive","Sechand_valuemotive","Sechand_imagemotive","Sechand_integrationmotive", "Sechand_pricemotive", "Socialapprop_sechand","Perc_injnorm_sechand",
          "Alcohol_hedonicmotive", "Alcohol_healthmotive", "Alcohol_valuemotive","Alcohol_imagemotive","Alcohol_integrationmotive", "Alcohol_pricemotive", "Socialapprop_Alcohol","Perc_injnorm_Alcohol",
          "Meat_hedonicmotive", "Meat_healthmotive", "Meat_valuemotive","Meat_imagemotive","Meat_integrationmotive", "Meat_pricemotive", "Socialapprop_Meat","Perc_injnorm_Meat",
          "session_1","session_2", "session_3", "session_4", "session_5", "session_6", "session_7", "session_8", "session_9", "session_10", "session_11", "session_12", "session_13", "session_14","session_15", "session_16", "session_17", "session_18", "session_19", "session_20",  "session_21",  "session_22",
          "answering_time","id")
)

for (i in 1:nrow(rename_table)) {
  names(data)[names(data) == rename_table$old[i]] <- rename_table$new[i]
}


#On recode les variables binaires en dummy (un/zéro)

dummies <- names(data)[sapply(data, function(x) all(na.omit(x) %in% c("Oui", "Non")))]
data[dummies] <- lapply(data[dummies], function(x) ifelse(x == "Oui", 1, 0))

dummies_threeitems <- names(data)[
  sapply(data, function(x) all(na.omit(x) %in% c("Oui", "Non", "Incertain","Peut-être/A confirmer")))
]
data[dummies_threeitems] <- lapply(
  data[dummies_threeitems],
  function(x) ifelse(x == "Oui", 1, 0)
)

#On convertit les NA en zéros
data[is.na(data)] <- 0

#et le temps de réponse en minutes
data$answering_time <- data$answering_time/60

#On convertit les variable catégorielles (hors Likert) en variables numériques
data$female_dummy <- ifelse(data$Gender == "Femme", 1, 0)
data$veg_dummy <- ifelse(data$Meateating_dummy == "0", 1, 0)

#Eventuellement faire de même pour d'autres variables par la suite (diplôme etc)


#(Dans la section suivante, on fait de même pour les variables Likert. 
#Comme les points attribués font l'objet d'une décision arbitraire, on effectue cette tâche dans la partie "Paramètres"; en prévision de tests de robustesse sur la construction de l'indice.)


# --- Paramètres ajustables (pour les tests de robustesse etc) ---

w_stateddonation_env <- 1   # pondération du don déclaré (en points par 10% dépensés)
w_statedplane_env <- 1 # idem pour la part déclarée dépensée dans l'avion (points négatifs car comportement polluant)

noplane_reward <- 30 # bonus si aucune dépense hypothétique dans l'avion (indépendamment des motifs)
greenmeal_reward <- 20  # bonus si le repas hypothétique choisi est le même que celui identifié comme plus vert
greenmeal_extrareward <- 10 # bonus supplémentaire si ce choix vert est effectué même s'il va à l'encontre des autres préférences (goût et santé)
reportedsecondhand_reward <- 5 # bonus si déclare acheter de seconde main (indépendamment des motifs et de la fréquence)

socials_imagepoints <- 40

w_reportedplane <- 4  # poids du malus pour le comportement rapporté "prendre l'avion", pondération de la fréquence
w_reportedmeat <- 2 #idem pour la viande (à rechanger par la suite!)

reportedplane_threshold <- 2  # fréquence annuelle seuil pour l'avion sous laquelle "valeurs personnelles" déclarées importantes peuvent soutenir un effort pro-environnemental (ex: 1 AR/an, soit 2 vols/an)
reportedmeat_threshold <- 7  # fréquence hebdomadaire seuil pour la viande sous laquelle "valeurs personnelles" déclarées importantes peuvent soutenir un effort pro-environnemental (ex: 1 fois par jour)


### Attention: une fois les paramètres changés, il faut recommencer depuis l'étape d'importation des données pour que l'indice soit modifié en conséquence !!!



# ---- Application des pondérations Likert avec dplyr ----
data <- data %>%
  
  # 1. Points Likert pour comportement vert : "Sechand"
  mutate(
    alpha_likert_points_sechand = case_when(
      Sechand_valuemotive == "Très important" ~ 10,
      Sechand_valuemotive == "Assez important" ~ 5,
      Sechand_valuemotive == "Moyennement important" ~ 0,
      Sechand_valuemotive == "Peu important" ~ -5,
      Sechand_valuemotive == "Pas du tout important" ~ -10,
      TRUE ~ NA_real_
    ),
    
    # 2. Points Likert pour comportement brun : avion
    alpha_likert_points_plane = case_when(
      Nb_flights_peryear < reportedplane_threshold & Plane_valuemotive == "Très important" ~ 10,
      Nb_flights_peryear < reportedplane_threshold & Plane_valuemotive == "Assez important" ~ 5,
      Nb_flights_peryear < reportedplane_threshold & Plane_valuemotive == "Moyennement important" ~ 0,
      Nb_flights_peryear < reportedplane_threshold & Plane_valuemotive == "Peu important" ~ -5,
      Nb_flights_peryear < reportedplane_threshold & Plane_valuemotive == "Pas du tout important" ~ -10,
      
      Nb_flights_peryear >= reportedplane_threshold & Plane_valuemotive == "Très important" ~ -8,
      Nb_flights_peryear >= reportedplane_threshold & Plane_valuemotive == "Assez important" ~ -6,
      Nb_flights_peryear >= reportedplane_threshold & Plane_valuemotive == "Moyennement important" ~ -4,
      Nb_flights_peryear >= reportedplane_threshold & Plane_valuemotive == "Peu important" ~ -2,
      Nb_flights_peryear >= reportedplane_threshold & Plane_valuemotive == "Pas du tout important" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # 3. Points Likert pour comportement brun : viande
    alpha_likert_points_meat = case_when(
      Nb_meatdishes_perweek < reportedmeat_threshold & Meat_valuemotive == "Très important" ~ 10,
      Nb_meatdishes_perweek < reportedmeat_threshold & Meat_valuemotive == "Assez important" ~ 5,
      Nb_meatdishes_perweek < reportedmeat_threshold & Meat_valuemotive == "Moyennement important" ~ 0,
      Nb_meatdishes_perweek < reportedmeat_threshold & Meat_valuemotive == "Peu important" ~ -5,
      Nb_meatdishes_perweek < reportedmeat_threshold & Meat_valuemotive == "Pas du tout important" ~ -10,
      
      Nb_meatdishes_perweek >= reportedmeat_threshold & Meat_valuemotive == "Très important" ~ -8,
      Nb_meatdishes_perweek >= reportedmeat_threshold & Meat_valuemotive == "Assez important" ~ -6,
      Nb_meatdishes_perweek >= reportedmeat_threshold & Meat_valuemotive == "Moyennement important" ~ -4,
      Nb_meatdishes_perweek >= reportedmeat_threshold & Meat_valuemotive == "Peu important" ~ -2,
      Nb_meatdishes_perweek >= reportedmeat_threshold & Meat_valuemotive == "Pas du tout important" ~ 0,
      TRUE ~ NA_real_
    )
  )

#4. Points Likert pour la sensibilité à l'image

data <- data %>%
  
  # 1. Points Likert pour comportement vert : "Sechand"
  mutate(
    beta_likert_points_sechand = case_when(
      Sechand_imagemotive == "Très important" ~ 16,
      Sechand_imagemotive == "Assez important" ~ 8,
      Sechand_imagemotive == "Moyennement important" ~ 4,
      Sechand_imagemotive == "Peu important" ~ 2,
      Sechand_imagemotive == "Pas du tout important" ~ -0,
      TRUE ~ NA_real_
    ),
    
    beta_likert_points_socials= case_when(
      Socials_imagemotive == "Très important" ~ 16,
      Socials_imagemotive == "Assez important" ~ 8,
      Socials_imagemotive == "Moyennement important" ~ 4,
      Socials_imagemotive == "Peu important" ~ 2,
      Socials_imagemotive == "Pas du tout important" ~ -0,
      TRUE ~ NA_real_
    ),
    
    beta_likert_points_plane= case_when(
      Plane_imagemotive == "Très important" ~ 16,
      Plane_imagemotive == "Assez important" ~ 8,
      Plane_imagemotive == "Moyennement important" ~ 4,
      Plane_imagemotive == "Peu important" ~ 2,
      Plane_imagemotive == "Pas du tout important" ~ -0,
      TRUE ~ NA_real_
    ),
    
    beta_likert_points_meat= case_when(
      Meat_imagemotive == "Très important" ~ 16,
      Meat_imagemotive == "Assez important" ~ 8,
      Meat_imagemotive == "Moyennement important" ~ 4,
      Meat_imagemotive == "Peu important" ~ 2,
      Meat_imagemotive == "Pas du tout important" ~ -0,
      TRUE ~ NA_real_
    ),
    
      beta_likert_points_alcohol= case_when(
      Alcohol_imagemotive == "Très important" ~ 16,
      Alcohol_imagemotive == "Assez important" ~ 8,
      Alcohol_imagemotive == "Moyennement important" ~ 4,
      Alcohol_imagemotive == "Peu important" ~ 2,
      Alcohol_imagemotive == "Pas du tout important" ~ -0,
      TRUE ~ NA_real_
    )
    
  )


# --- Construction des indices alpha et beta ---

# 1. Conversion des dépenses hypothétiques déclarées en points 
data <- data %>%
  mutate(
    stateddonation_envpoints = (as.numeric(Donation_share) / 10) * w_stateddonation_env,
    statedplane_envpoints    = -(as.numeric(Plane_share) / 10) * w_statedplane_env
    )

# 2. Bonus absence de dépense "avion" et choix de plats hypothétiques pro-environnementaux pour alpha

data <- data %>% mutate(noplane_reward = if_else(Plane_share == 0, noplane_reward, 0))%>%
  mutate(
    greenmeal_reward = if_else(
      Fav_hypdish_environment == Chosen_hypdish,
      greenmeal_reward,
      0
    ),
    greenmeal_extrareward = if_else(
      Fav_hypdish_environment == Chosen_hypdish &
        Fav_hypdish_environment != Fav_hypdish_taste &
        Fav_hypdish_environment != Fav_hypdish_health,
      greenmeal_extrareward,
      0
    )
  )


# 3. Points de pratique pondérés (ou non) pas la fréquence de pratique

## Points non pondérés par la fréquence de pratique (seconde-main pour environnement et autres pour image)

data <- data %>%
  mutate(
    # Bonus seconde main
    secondhand_reward = if_else(
      Buying_sechand_dummy == 1,
      reportedsecondhand_reward,
      0
    ),
    
    # Points image réseaux sociaux
    socials_imagereward = if_else(
      Posting_socials_dummy == 1, 
      socials_imagepoints, 
      0
    )
  )
    

  

## Points pondérés par la fréquence de pratique (avion et viande)
data <- data %>%
  mutate(
    # Malus avion pondéré par vol déclaré
    plane_malus = -w_reportedplane * Nb_flights_peryear,
    # Malus viande pondéré par repas carné par semaine
    meat_malus = -w_reportedmeat * Nb_meatdishes_perweek
  )



### 4. Vérification de la cohérence interne des indices composites (alpha de Cronbach) ###

alpha_items <- data[, c("alpha_likert_points_sechand",
                        "alpha_likert_points_plane",
                        "alpha_likert_points_meat",
                        "stateddonation_envpoints",
                        "statedplane_envpoints",
                        "noplane_reward",
                        "greenmeal_reward",
                        "greenmeal_extrareward",
                        "secondhand_reward",
                        "plane_malus",
                        "meat_malus")]

beta_items <- data[, c("beta_likert_points_sechand",
                       "beta_likert_points_plane",
                       "beta_likert_points_meat",
                       "beta_likert_points_alcohol",
                       "beta_likert_points_socials",
                       "socials_imagereward")]



# --- Fonction complète : alpha de Cronbach + diagnostic visuel ---
cronbach_diagnostics <- function(df, index_name = "Indice") {
  # Nettoyage
  df <- na.omit(df)
  k <- ncol(df)
  
  # Calcul de l'alpha
  compute_alpha <- function(x) {
    var_items <- apply(x, 2, var)
    var_total <- var(rowSums(x))
    (ncol(x) / (ncol(x) - 1)) * (1 - sum(var_items) / var_total)
  }
  
  alpha_global <- compute_alpha(df)
  
  # Calcul alpha sans chaque item
  alpha_without <- sapply(names(df), function(col) {
    compute_alpha(df[, setdiff(names(df), col)])
  })
  
  delta_alpha <- alpha_without - alpha_global
  
  results <- data.frame(
    Item = names(df),
    Alpha_sans_item = round(alpha_without, 3),
    Variation = round(delta_alpha, 3),
    Effet = ifelse(delta_alpha > 0, "↗ améliore cohérence", "↘ réduit cohérence")
  )
  
  # --- Affichage texte ---
  cat("\n--- Diagnostic de cohérence interne pour :", index_name, "---\n")
  cat("Alpha global :", round(alpha_global, 3), "\n\n")
  print(results, row.names = FALSE)
  cat("\nItems avec Variation > 0 → potentiellement incohérents.\n")
  
  # --- Visualisation ---
  library(ggplot2)
  ggplot(results, aes(x = reorder(Item, Variation), y = Variation, fill = Effet)) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Impact des items sur la cohérence interne (", index_name, ")", sep = ""),
      subtitle = paste("Alpha global =", round(alpha_global, 3)),
      x = "Item retiré",
      y = "Variation de l'alpha"
    ) +
    scale_fill_manual(values = c("↗ améliore cohérence" = "tomato", "↘ réduit cohérence" = "seagreen3")) +
    theme_minimal(base_size = 13)
  
  return(invisible(results))
}

cronbach_diagnostics(alpha_items)
cronbach_diagnostics(beta_items)



# 5. Indices de sensibilité environnementale et à l'image sociale

data$alpha_score <- rowSums(data[, c("alpha_likert_points_sechand", "alpha_likert_points_plane", "alpha_likert_points_meat", 
                                     "stateddonation_envpoints", "statedplane_envpoints", "noplane_reward", "greenmeal_reward", "greenmeal_extrareward", "secondhand_reward",
                                     "plane_malus","meat_malus")], na.rm = TRUE)
data$alpha_measured <- data$alpha_score/100

data$beta_score <- rowSums(data[, c("beta_likert_points_sechand", "beta_likert_points_plane", "beta_likert_points_meat", "beta_likert_points_alcohol", "beta_likert_points_socials",
                                    "socials_imagereward")], na.rm = TRUE)
data$beta_measured <- data$beta_score/100


summary(data$alpha_measured)
summary(data$beta_measured)







####### Test constitution de sessions équilibrées ##############

### Méthode 2: Sessions selon contrainte de disponbilité puis méthode du Cube ####

set.seed(123)

# --------------------------------------------
# 1. Simuler une base avec 20 sessions et 400 participants
# --------------------------------------------

N <- 400
S <- 20

participants <- tibble(
  id = 1:N,
  alpha = runif(N),
  alpha_tercile = factor(ntile(alpha, 3), labels = c("low", "mid", "high")),
  gender = sample(c("F","M"), N, replace = TRUE, prob = c(0.6, 0.4)),
  food_id = sample(c("omnivore","veg"), N, replace = TRUE, prob = c(0.7, 0.3))
)

# Dispo simulée : 1 = dispo, 0 = pas dispo
set.seed(123)
availability_matrix <- matrix(rbinom(N*S, 1, 0.25), nrow=N, ncol=S)
colnames(availability_matrix) <- paste0("Session_", 1:S)

participants <- bind_cols(participants, as_tibble(availability_matrix))

# --------------------------------------------
# 2. Construction des sessions (SES clusters)
# --------------------------------------------
SESSION_SIZE <- 20
sessions <- vector("list", S)

remaining <- participants

for (k in 1:S) {
  
  available <- remaining %>% filter(.data[[paste0("Session_", k)]] == 1)
  session <- head(available, SESSION_SIZE)
  
  sessions[[k]] <- session
  
  # retirer les assignés
  remaining <- remaining %>% filter(!id %in% session$id)
}

# Vérifier tailles
sapply(sessions, nrow)  #On remarque que les dernières sessions ne sont pas pleines



### Méthode 2 améliorée (fonctionnelle) ####

library(lpSolve)
library(dplyr)
library(tibble)
library(sampling)
library(readr)

# ---------- Paramètres ----------
U <- 20          # capacité max par session
MIN_SIZE <- 16   # capacité min par session ouverte
epsilon <- 1e-3  # pénalité pour ouvrir plus de sessions
set.seed(123)

# participants: data.frame avec id, gender, food_id, alpha
# A: N x S matrice de disponibilité (0/1)
N <- nrow(participants)
S <- ncol(A)

# ---------- MILP pour composer les sessions ----------
num_x <- N * S
num_y <- S
num_vars <- num_x + num_y

# Objectif: max nombre assigné - epsilon * nombre de sessions ouvertes
obj <- c(rep(1, num_x), rep(-epsilon, num_y))

constr_mat <- list()
constr_dir <- c()
constr_rhs <- c()

# (1) Chaque participant max 1 session
for (i in 1:N) {
  row <- rep(0, num_vars)
  row[((i-1)*S + 1):(i*S)] <- 1
  constr_mat[[length(constr_mat)+1]] <- row
  constr_dir <- c(constr_dir, "<=")
  constr_rhs <- c(constr_rhs, 1)
}

# (2) Capacité max par session
for (s in 1:S) {
  row <- rep(0, num_vars)
  row[seq(s, num_x, by=S)] <- 1
  row[num_x + s] <- -U
  constr_mat[[length(constr_mat)+1]] <- row
  constr_dir <- c(constr_dir, "<=")
  constr_rhs <- c(constr_rhs, 0)
}

# (3) Capacité min par session ouverte
for (s in 1:S) {
  row <- rep(0, num_vars)
  row[seq(s, num_x, by=S)] <- -1
  row[num_x + s] <- MIN_SIZE
  constr_mat[[length(constr_mat)+1]] <- row
  constr_dir <- c(constr_dir, "<=")
  constr_rhs <- c(constr_rhs, 0)
}

# (4) Disponibilité
for (i in 1:N) {
  for (s in 1:S) {
    if (A[i,s] == 0) {
      row <- rep(0, num_vars)
      row[(i-1)*S + s] <- 1
      constr_mat[[length(constr_mat)+1]] <- row
      constr_dir <- c(constr_dir, "<=")
      constr_rhs <- c(constr_rhs, 0)
    }
  }
}

# Résolution MILP
const_mat <- do.call(rbind, constr_mat)
sol <- lp(direction = "max",
          objective.in = obj,
          const.mat = const_mat,
          const.dir = constr_dir,
          const.rhs = constr_rhs,
          all.bin = TRUE)

if (sol$status != 0) stop("MILP infeasible")

solution <- sol$solution
x_sol <- matrix(solution[1:num_x], nrow = N, ncol = S, byrow = TRUE)
y_sol <- solution[(num_x + 1):(num_x + num_y)]
opened_sessions <- which(y_sol == 1)

# ---------- Construire les sessions ----------
assignments <- which(x_sol == 1, arr.ind = TRUE)
assign_df <- tibble(
  row_in_participants = assignments[,1],
  session_index = assignments[,2]
) %>%
  mutate(
    id = participants$id[row_in_participants],
    session = paste0("Session_", session_index)
  ) %>%
  select(id, session)

# ---------- Covariables par session ----------
session_cov <- assign_df %>%
  left_join(participants %>% select(id, gender, food_id, alpha), by = "id") %>%
  group_by(session) %>%
  summarise(
    n_F = sum(gender == "F"),
    n_veg = sum(food_id == "veg"),
    n_high_alpha = sum(alpha > quantile(alpha, 2/3)),
    n_total = n(),
    .groups = "drop"
  )

X <- as.matrix(session_cov %>% select(n_F, n_veg, n_high_alpha))
pik <- rep(0.5, nrow(session_cov)) # probabilité de traitement = 0.5
session_cov$pik <- pik

# ---------- Assignation traitement / contrôle ----------
set.seed(123)
treatment_assignment <- samplecube(pik = session_cov$pik, X = X)
session_cov <- session_cov %>%
  mutate(
    treated = treatment_assignment,
    control = 1 - treated
  )

# ---------- Appliquer aux participants ----------
assign_df <- assign_df %>%
  left_join(session_cov %>% select(session, treated), by = "session") %>%
  mutate(treatment = treated) %>%
  select(id, session, treatment)

# ---------- Diagnostics d'équilibrage ----------
balance_df <- data.frame(
  cov = colnames(X),
  mean_treated = colSums(X * session_cov$treated) / sum(session_cov$treated * session_cov$n_total),
  mean_control = colSums(X * session_cov$control) / sum(session_cov$control * session_cov$n_total)
)
balance_df <- balance_df %>%
  mutate(
    diff_std = (mean_treated - mean_control) / sqrt((mean_treated*(1-mean_treated) + mean_control*(1-mean_control))/2)
  )
print(balance_df)

# ---------- Export de la liste de randomisation ----------
write_csv(assign_df, "randomisation_list.csv")
cat("Randomisation exportée dans 'randomisation_list.csv'\n")










