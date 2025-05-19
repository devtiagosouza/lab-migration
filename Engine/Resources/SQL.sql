SET TERM ^ ;



/******************************************************************************/
/****                         Triggers for tables                          ****/
/******************************************************************************/



/* Trigger: ABASTECIMENTO_BI */
CREATE OR ALTER TRIGGER ABASTECIMENTO_BI FOR ABASTECIMENTO
ACTIVE BEFORE INSERT POSITION 0
AS
 BEGIN
   IF (NEW.ID_AB IS NULL) THEN
     NEW.ID_AB = GEN_ID(GEN_ABASTECIMENTO_ID,1);
 END
^

/* Trigger: ABASTECIMENTO_ESTOQUE_TANQUES */
CREATE OR ALTER TRIGGER ABASTECIMENTO_ESTOQUE_TANQUES FOR ABASTECIMENTO
ACTIVE AFTER INSERT OR UPDATE OR DELETE POSITION 0
AS
     --Depois
      declare variable est_atual numeric(15,3);
      declare variable id_tanque integer;
    begin
      id_tanque = 0;
      select  t.id_tanque from tanques t where COALESCE(t.tq_ativo,'S') <> 'N'
      AND  t.tq_tanque = new.tanque into :id_tanque;

      select  coalesce(tq_estoque_escritural,0) from tanques t where t.id_tanque = :id_tanque into :est_atual;


      IF (INSERTING) THEN BEGIN  --DEPOIS DE INSERIR

            UPDATE ABASTECIMENTO AB SET AB.estoque_inicial = :est_atual WHERE AB.id_ab = NEW.id_ab;

            update tanques t set t.tq_estoque_escritural = t.tq_estoque_escritural - new.litro where t.id_tanque = :id_tanque;

      END
      ELSE if (UPDATING)  then BEGIN
            IF ((old.status = 'PENDENTE') AND (NEW.status = 'AFERICAO')) then begin  --SE ESTIVER MUDANDO O STATUS DE PENDENTE PARA AFERICAO DEVOLVE O ESTOQUE
               update tanques t set t.tq_estoque_escritural = t.tq_estoque_escritural + old.litro WHERE t.id_tanque = :id_tanque;
            END

            IF (OLD.litro <> NEW.litro) then BEGIN --EST? ATUALIZANDO A QUANTIDADE. ? O CASO DE PROBLEMA DE CASAS DECIMAIS NO CONCENTRADOR QUE ? PRECISO
               --VOLTAR O CORRETO

               --VOLTA O ESTOQUE ANTIGO
                update tanques t set t.tq_estoque_escritural = t.tq_estoque_escritural + old.litro WHERE T.id_tanque = :id_tanque;
                --BAIXA A QUANTIDADE NOVA
                update tanques t set t.tq_estoque_escritural = t.tq_estoque_escritural - new.litro WHERE T.id_tanque = :id_tanque;
            END
      END
      ELSE if (DELETING) then BEGIN  --DEVOLVE O ESTOQUE
          if (old.status <> 'AFERICAO') then
               update tanques t set t.tq_estoque_escritural = t.tq_estoque_escritural + old.litro WHERE  T.id_tanque = :id_tanque;
      END
    END
^
