// Fonction Power Query (M) pour nettoyer et parser un nom complet francophone
// Usage:
//   fxNettoyerParserNom("Mme Tremblay J Marie-Claire PhD")
//   fxNettoyerParserNom([NomComplet], PrenomsQC_CAN[Prenom], NomsQC_CAN[NomFamille])
let
    fxNettoyerParserNom = (
        NomComplet as nullable text,
        optional PrenomsDictionnaire as nullable list,
        optional NomsDictionnaire as nullable list
    ) as record =>
    let
        PrefixesTable = #table(
            type table [Prefixe = text, Canonique = text],
            {
                {"mr", "M."}, {"m", "M."}, {"monsieur", "M."},
                {"mme", "Mme"}, {"madame", "Mme"},
                {"mlle", "Mlle"}, {"mademoiselle", "Mlle"},
                {"dr", "Dr"}, {"docteur", "Dr"}, {"prof", "Pr"},
                {"pr", "Pr"}, {"sir", "Sir"}, {"lady", "Lady"}
            }
        ),

        SuffixesTable = #table(
            type table [Suffixe = text, Canonique = text],
            {
                {"phd", "PhD"}, {"ph.d", "PhD"}, {"ph.d.", "PhD"},
                {"msc", "MSc"}, {"m.sc", "MSc"}, {"m.sc.", "MSc"},
                {"md", "MD"}, {"m.d", "MD"}, {"m.d.", "MD"},
                {"mba", "MBA"}, {"cpa", "CPA"}, {"ing", "ing."},
                {"dr", "Dr"}, {"jr", "Jr"}, {"sr", "Sr"}
            }
        ),

        DefaultPrenoms = {
            "Alexandre", "Alexis", "Alice", "Amélie", "André", "Antoine", "Ariane", "Audrey", "Benoît", "Camille",
            "Caroline", "Catherine", "Charles", "Charlotte", "Chloé", "Clara", "David", "Dominique", "Édouard", "Éliane",
            "Élise", "Élodie", "Emma", "Étienne", "Evan", "Félix", "Florence", "Francis", "Gabriel", "Geneviève",
            "Guillaume", "Hugo", "Isabelle", "Jacques", "Jean", "Jean-François", "Jérôme", "Julie", "Justine", "Karine",
            "Laurence", "Laurent", "Léa", "Léo", "Léonie", "Louis", "Louise", "Luc", "Marc", "Marie",
            "Marie-Ève", "Martin", "Mathieu", "Maxime", "Mélanie", "Michaël", "Michel", "Nadia", "Nathalie", "Nicolas",
            "Noah", "Olivier", "Pascal", "Patrick", "Philippe", "Raphaël", "Rémi", "Robert", "Samuel", "Sébastien",
            "Simon", "Sonia", "Sophie", "Stéphane", "Valérie", "Vincent", "William", "Yves"
        },

        DefaultNoms = {
            "Bouchard", "Gagnon", "Côté", "Roy", "Tremblay", "Fortin", "Gauthier", "Morin", "Lavoie", "Pelletier",
            "Bélanger", "Lévesque", "Bergeron", "Caron", "Beaulieu", "Cloutier", "Paquet", "Nadeau", "Girard", "Poirier",
            "Martel", "Paré", "Boudreau", "Dufour", "Rousseau", "Michaud", "Couture", "Arsenault", "Pelletier", "Landry",
            "Dubé", "Desjardins", "Hébert", "Savard", "Mercier", "Gendron", "Charbonneau", "Lambert", "Allard", "Leduc",
            "Parent", "Boivin", "Laflamme", "Brunet", "Renaud", "Lalonde", "Lemieux", "Pigeon", "Lapointe", "Blais"
        },

        PrenomsRef = if PrenomsDictionnaire <> null and List.Count(PrenomsDictionnaire) > 0 then List.Buffer(PrenomsDictionnaire) else List.Buffer(DefaultPrenoms),
        NomsRef = if NomsDictionnaire <> null and List.Count(NomsDictionnaire) > 0 then List.Buffer(NomsDictionnaire) else List.Buffer(DefaultNoms),

        TrimMultiSpaces = (t as text) as text => Text.Combine(List.Select(Text.SplitAny(Text.Trim(t), " \t\r\n"), each _ <> ""), " "),

        NormalizeApostrophes = (t as text) as text =>
            Text.Replace(
                Text.Replace(
                    Text.Replace(t, "’", "'"),
                    "`", "'"
                ),
                "´", "'"
            ),

        ProperToken = (t as text) as text =>
            let
                HyphenParts = Text.Split(t, "-"),
                ProperHyphen = List.Transform(
                    HyphenParts,
                    each
                        let
                            ApParts = Text.Split(_, "'"),
                            ProperAp = List.Transform(ApParts, each Text.Proper(Text.Lower(_)))
                        in
                            Text.Combine(ProperAp, "'")
                )
            in
                Text.Combine(ProperHyphen, "-"),

        ToProperName = (t as nullable text) as nullable text =>
            if t = null or Text.Trim(t) = "" then null else Text.Combine(List.Transform(Text.Split(t, " "), each ProperToken(_)), " "),

        Clean0 = if NomComplet = null then "" else NomComplet,
        Clean1 = TrimMultiSpaces(Clean0),
        Cleaned = NormalizeApostrophes(Clean1),

        NicknameQuoted =
            let
                q1 = Text.BetweenDelimiters(Cleaned, "\"", "\"", 0, 0),
                q2 = Text.BetweenDelimiters(Cleaned, "'", "'", 0, 0)
            in
                if q1 <> null and Text.Trim(q1) <> "" then q1 else if q2 <> null and Text.Trim(q2) <> "" then q2 else null,

        NicknameParen = Text.BetweenDelimiters(Cleaned, "(", ")", 0, 0),
        NomUsuel = if NicknameParen <> null and Text.Trim(NicknameParen) <> "" then Text.Trim(NicknameParen) else if NicknameQuoted <> null and Text.Trim(NicknameQuoted) <> "" then Text.Trim(NicknameQuoted) else null,

        WithoutParen = Text.BeforeDelimiter(Cleaned, "(", 0),
        WithoutQuotes = Text.Replace(Text.Replace(WithoutParen, "\"", ""), "'", ""),
        TokensRaw = List.Select(List.Transform(Text.Split(TrimMultiSpaces(WithoutQuotes), " "), each Text.Trim(_)), each _ <> ""),
        TokensLower = List.Transform(TokensRaw, each Text.Lower(_)),

        PrefixMatch = if List.Count(TokensLower) > 0 then Table.SelectRows(PrefixesTable, each [Prefixe] = TokensLower{0}) else #table(type table [Prefixe = text, Canonique = text], {}),
        Salutation = if Table.RowCount(PrefixMatch) > 0 then PrefixMatch{0}[Canonique] else null,

        TokensNoPrefix = if Salutation <> null and List.Count(TokensRaw) > 1 then List.Skip(TokensRaw, 1) else TokensRaw,
        TokensNoPrefixLower = List.Transform(TokensNoPrefix, each Text.Lower(_)),

        LastTokenLower = if List.Count(TokensNoPrefixLower) > 0 then TokensNoPrefixLower{List.Count(TokensNoPrefixLower)-1} else null,
        SuffixMatch = if LastTokenLower <> null then Table.SelectRows(SuffixesTable, each [Suffixe] = LastTokenLower) else #table(type table [Suffixe = text, Canonique = text], {}),
        Titre = if Table.RowCount(SuffixMatch) > 0 then SuffixMatch{0}[Canonique] else null,

        TokensCore = if Titre <> null and List.Count(TokensNoPrefix) > 1 then List.RemoveLastN(TokensNoPrefix, 1) else TokensNoPrefix,

        IsInitial = (x as text) as logical =>
            let p = Text.Replace(x, ".", "") in Text.Length(p) = 1,

        Initiale =
            if List.Count(TokensCore) >= 2 and IsInitial(TokensCore{1}) then Text.Upper(Text.Start(Text.Replace(TokensCore{1}, ".", ""), 1))
            else null,

        CoreNoInitial = if Initiale <> null and List.Count(TokensCore) >= 2 then List.FirstN(TokensCore, 1) & List.Skip(TokensCore, 2) else TokensCore,

        CandidateNomA = if List.Count(CoreNoInitial) >= 1 then CoreNoInitial{0} else null,
        CandidatePrenomA = if List.Count(CoreNoInitial) >= 2 then Text.Combine(List.Skip(CoreNoInitial, 1), " ") else null,

        CandidatePrenomB = if List.Count(CoreNoInitial) >= 1 then CoreNoInitial{0} else null,
        CandidateNomB = if List.Count(CoreNoInitial) >= 2 then Text.Combine(List.Skip(CoreNoInitial, 1), " ") else null,

        ExactInList = (value as nullable text, dict as list) as logical => if value = null then false else List.Contains(List.Transform(dict, each Text.Lower(_)), Text.Lower(value)),

        InversionCorrigee =
            if CandidatePrenomB <> null and CandidateNomB <> null and ExactInList(CandidatePrenomB, PrenomsRef) and ExactInList(CandidateNomB, NomsRef)
            then true else false,

        NomFamille0 = if InversionCorrigee then CandidateNomB else CandidateNomA,
        Prenom0 = if InversionCorrigee then CandidatePrenomB else CandidatePrenomA,

        ExactLookup = (value as nullable text, dict as list) as nullable text =>
            if value = null then null
            else
                let
                    t = Table.FromList(dict, Splitter.SplitByNothing(), {"ref"}),
                    f = Table.SelectRows(t, each Text.Lower([ref]) = Text.Lower(value))
                in
                    if Table.RowCount(f) > 0 then f{0}[ref] else null,

        FuzzyLookup = (value as nullable text, dict as list) as nullable text =>
            if value = null then null
            else
                let
                    i = #table(type table [v = text], {{value}}),
                    d = Table.FromList(dict, Splitter.SplitByNothing(), {"ref"}),
                    j = Table.FuzzyNestedJoin(i, {"v"}, d, {"ref"}, "m", JoinKind.LeftOuter, [IgnoreCase = true, IgnoreSpace = true, Threshold = 0.76]),
                    e = if Table.RowCount(j) = 0 then #table(type table [ref = text], {}) else j{0}[m]
                in
                    if Table.RowCount(e) > 0 then e{0}[ref] else null,

        PrenomExact = ExactLookup(Prenom0, PrenomsRef),
        NomExact = ExactLookup(NomFamille0, NomsRef),

        PrenomFuzzy = if PrenomExact = null then FuzzyLookup(Prenom0, PrenomsRef) else PrenomExact,
        NomFuzzy = if NomExact = null then FuzzyLookup(NomFamille0, NomsRef) else NomExact,

        PrenomFinal = ToProperName(if PrenomExact <> null then PrenomExact else PrenomFuzzy),
        NomFinal = ToProperName(if NomExact <> null then NomExact else NomFuzzy),

        MethodePrenom = if PrenomExact <> null then "exact" else if PrenomFuzzy <> null then "fuzzy" else null,
        MethodeNom = if NomExact <> null then "exact" else if NomFuzzy <> null then "fuzzy" else null
    in
        [
            TexteNettoye = Cleaned,
            Salutation = Salutation,
            NomFamille = NomFinal,
            Initiale = Initiale,
            Prenom = PrenomFinal,
            Titre = Titre,
            NomUsuel = ToProperName(NomUsuel),
            InversionCorrigee = InversionCorrigee,
            MethodeCorrespondancePrenom = MethodePrenom,
            MethodeCorrespondanceNom = MethodeNom,
            PrefixesTable = PrefixesTable,
            SuffixesTable = SuffixesTable
        ]
in
    fxNettoyerParserNom
