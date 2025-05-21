CREATE OR ALTER TRIGGER NFCE_BI FOR NFCE
ACTIVE BEFORE INSERT POSITION 0
AS
 DECLARE VARIABLE ambiente INTEGER; /* VERSAO:1.0 */
BEGIN
  IF (NEW.id_nfce IS NULL) THEN
    NEW.id_nfce = GEN_ID(gen_nfce_id,1);

    SELECT c.tipo_ambiente FROM configuracao_nfce c INTO : ambiente;

    IF (GEN_ID(gen_serie_nfce_producao,0) = 0) THEN
      EXECUTE STATEMENT 'set generator gen_serie_nfce_producao to 1';

    IF (GEN_ID(gen_serie_nfce_homologacao,0) = 0) THEN
      EXECUTE STATEMENT 'set generator gen_serie_nfce_homologacao to 1';

    IF (ambiente = 1) THEN BEGIN /*PRODU??O*/
       NEW.numero = GEN_ID(gen_numero_nfce_producao,1);
       IF (NEW.numero > 999999999) THEN BEGIN
          NEW.numero = 1;
          EXECUTE STATEMENT 'set generator gen_numero_nfce_producao to 1'; /*retorna para 1*/
          NEW.serie = GEN_ID(gen_serie_nfce_producao,1);   /*incrementa mais um na s?rie*/
       END
       ELSE NEW.serie = GEN_ID(gen_serie_nfce_producao,0);
    END
    ELSE BEGIN    /*HOMOLOGA??O*/
       NEW.numero = GEN_ID(gen_numero_nfce_homologacao,1);
       IF (NEW.numero > 999999999) THEN BEGIN
          NEW.numero = 1;
          EXECUTE STATEMENT 'set generator gen_numero_nfce_homologacao to 1'; /*retorna para 1*/
          NEW.serie = GEN_ID(gen_serie_nfce_homologacao,1);   /*incrementa mais um na s?rie*/
       END
       ELSE NEW.serie = GEN_ID(gen_serie_nfce_homologacao,0);
    END


END
^
SET TERM ; ^
