DROP TABLE Ligue ;
DROP TABLE Equipe ;
DROP TABLE Joueur ;
DROP TABLE Match;
DROP TABLE Saison ;
DROP TABLE Engager ;


CREATE TABLE Ligue (
  ligueId INT,
  pays VARCHAR(100),
  nom_ligue VARCHAR2(100)
);


CREATE TABLE Equipe (
  equipeId INT PRIMARY KEY,
  nom_equipe VARCHAR2(100),
  nom_court VARCHAR2(3),
  ligueId INT,
  FOREIGN KEY(ligueId) REFERENCES Ligue(ligueId)
);

CREATE TABLE Joueur (
  joueurId INT PRIMARY KEY,
  nom_joueur VARCHAR2(100),
  prenom_joueur VARCHAR2(100),
  dateNaissance date,
  taille int,
  poids INT
);


CREATE TABLE games (
  matchId int PRIMARY KEY,
  dategames date,
  scoreLocale int,
  scoreVisiteur int,
  equipeLocale int,
  equipeVisiteur int,
  FOREIGN KEY(equipeLocale) REFERENCES Equipe(equipeId),
  FOREIGN KEY(equipeVisiteur) REFERENCES Equipe(equipeId)
);


CREATE TABLE Saison (
  saisonId int PRIMARY KEY,
  libelle VARCHAR2(100),
  dateDeb date,
  dateFin date
);

CREATE TABLE Engager (
  equipeId int,
  joueurId int,
  saisonId int
);
