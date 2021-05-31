unit umotelog;

{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, Variants, DB;
const
  nDESCL_TEXTO_SERVIDOR_APP = 40;
  STRING_INDEFINIDO = '';

type
  TMoteLogMetodoSemParametros = procedure of object;
  TMoteLogMetodoUmString = procedure(psParam1: string) of object;
  TMoteLogMetodoUmConstString = procedure(const psParam1: string) of object;
  TMoteLogMetodoDoisConstString = procedure(const psParam1: string;
    const psParam2: string) of object;
  TMoteLogMetodoUmConstStringUmConstBool = procedure(const psParam1: string;
    const pbParam2: boolean) of object;
  TMoteLogMetodoUmConstBool = procedure(const pbParam1: boolean) of object;
  TMoteLogMetodoUmConstInt = procedure(const pnParam1: integer) of object;
  TMoteLogMetodoUmConstObj = procedure(const poParam1: TObject) of object;
  TMoteLogMetodoUmVariant = procedure(const pvParam1: variant) of object;


  TprcNivelLog = (nlErro, nlAviso, nlInfo, nlDebug);
  TMoteLog = class
    ;

  IMoteContextoRegistroLog = interface
    ['{64848779-C608-4AEF-A9CF-AB8E56E21433}']
    function GetTexto: string;
    procedure SetTexto(const Value: string);
    function GetNivel: TprcNivelLog;
    procedure SetNivel(const Value: TprcNivelLog);
    function GetEntradaMetodo: boolean;
    procedure SetEntradaMetodo(const Value: boolean);
    function GetDataHoraCriacao: TDatetime;
    procedure SetDataHoraCriacao(const Value: TDatetime);
    function GetLog: TMoteLog;
    procedure SetLog(const Value: TMoteLog);
    procedure GerarLogRetornoMetodo(const psRetorno: string = 'void';
      const psTexto: string = STRING_INDEFINIDO);
    procedure GerarLogRetornoMetodoComErro;

    property prcTexto: string read GetTexto write SetTexto;
    property prcNivel: TprcNivelLog read GetNivel write SetNivel;
    property prcEntradaMetodo: boolean read GetEntradaMetodo write SetEntradaMetodo;
    property prcDataHoraCriacao: TDatetime read GetDataHoraCriacao write SetDataHoraCriacao;
    property motelog: TMoteLog read GetLog write SetLog;
  end;


  TprlContextoRegistroLog = class(TInterfacedObject, IMoteContextoRegistroLog)
    FsTexto: string;
    FenNivel: TprcNivelLog;
    FbEntradaMetodo: boolean;
    FdtDataHoraCriacao: TDatetime;
    FoLog: TMoteLog;
  private
    function GetTexto: string;
    procedure SetTexto(const Value: string);
    function GetNivel: TprcNivelLog;
    procedure SetNivel(const Value: TprcNivelLog);
    function GetEntradaMetodo: boolean;
    procedure SetEntradaMetodo(const Value: boolean);
    function GetDataHoraCriacao: TDatetime;
    procedure SetDataHoraCriacao(const Value: TDatetime);
    function GetLog: TMoteLog;
    procedure SetLog(const Value: TMoteLog);
  public
    procedure GerarLogRetornoMetodo(const psRetorno: string = 'void';
      const psTexto: string = STRING_INDEFINIDO);
    procedure GerarLogRetornoMetodoComErro;

    property prcTexto: string read GetTexto write SetTexto;
    property prcNivel: TprcNivelLog read GetNivel write SetNivel;
    property prcEntradaMetodo: boolean read GetEntradaMetodo write SetEntradaMetodo;
    property prcDataHoraCriacao: TDatetime read GetDataHoraCriacao write SetDataHoraCriacao;
    constructor Create(const psTexto: string; const penNivel: TprcNivelLog;
      const pbEntradaMetodo: boolean = False);
  end;

  TMoteLog = class
  private
    FvSpDB: olevariant;
    FoArquivoLog: TextFile;
    FsUsuarioLogado: string;
    FsUsuariosParaLog: string;
    FbLogAtivo: boolean;
    FenNivelLog: TprcNivelLog;
    FnNivelIndetacao: integer;
    FnNivelIndentacaoLimite: integer;
    FnQtdDatasetSalvos: integer;
    FsNomeArquivo: string;
    FsPastaLog: string;
    FbSalvarDataSets: boolean;
    FbAdicionarTempoMetodo: boolean;
    FnDelocamentolIndetacaoNoArquivo: integer;
    FsUltimoTextoInicioBloco: string;
    procedure LerIni;
    procedure DefinirSeLogEstaAtivo;
    procedure IniciarArquivoLog;
    function CalcularTextoDifTempo(const poContexto: IMoteContextoRegistroLog): string;
  protected
    property prcDelocamentolIndetacaoNoArquivo: integer
      read FnDelocamentolIndetacaoNoArquivo write FnDelocamentolIndetacaoNoArquivo;
  public
    constructor Create(const pvSpDB: olevariant; const psUsuarioLogado: string);
    destructor Destroy; override;
    function GerarLog(const psTexto: string; const penNivelLog: TprcNivelLog;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

    function GerarLogFmt(const psStringFormatacao: string;
      const poDadosParametrosFormat: array of const; const penNivelLog: TprcNivelLog = nlDebug;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

    function GerarLogRetorno(const poContexto: IMoteContextoRegistroLog;
      const psRetorno: string = 'void';
      const psTexto: string = STRING_INDEFINIDO): IMoteContextoRegistroLog;

    procedure GerarLogRetornoMetodo(const psRetorno: string = 'void';
      const psTexto: string = STRING_INDEFINIDO);

    procedure GerarLogRetornoMetodoComErro;

    function GerarLogRetornoFmt(const poContexto: IMoteContextoRegistroLog;
      const psRetorno: string; const psStringFormatacao: string;
      const poDadosParametrosFormat: array of const): IMoteContextoRegistroLog;

    function GerarLogRetornoBool(const poContexto: IMoteContextoRegistroLog;
      const pbRetorno: boolean; const psTexto: string = STRING_INDEFINIDO): IMoteContextoRegistroLog;

    function GerarLogErro(const psTexto: string;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

    function GerarLogExcecao(const psTextoAdicional: string = STRING_INDEFINIDO): IMoteContextoRegistroLog;

    function GerarLogAviso(const psTexto: string;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

    function GerarLogInfo(const psTexto: string;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

    function GerarLogDebug(const psTexto: string;
      const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
    procedure RecarregarAtualizacoes;

    //13/03/2019 - maxback - Defect 177476 -
    property motelogAtivo: boolean read FbLogAtivo;

  end;


function GeTMoteLog(const pvSpDB: olevariant; const psUsuarioLogado: string;
  const pbEstaNoServidor: boolean = False): TMoteLog; overload;

function GeTMoteLog(const pvSpDB: olevariant; const pbEstaNoServidor: boolean = False): TMoteLog;
  overload;

function motelog(const psNomeMetodoComClasse: string = ''; const psTextoParaLog: string = '';
  const pbInicioMetodo: boolean = False): IMoteContextoRegistroLog; overload;

function motelogfmt(const psStringFormatacao: string; const poDadosParametrosFormat: array of const;
  const penNivelLog: TprcNivelLog = nlDebug;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;

procedure motelogexception(const psTextoAdicional: string = STRING_INDEFINIDO);

procedure motelog(const poMetodo: TMoteLogMetodoSemParametros; const psNomeMetodo: string = '';
  const psTextoParaLog: string = ''); overload;

procedure motelog(const poMetodo: TMoteLogMetodoUmConstString; const psParam1: string;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
procedure motelog(const poMetodo: TMoteLogMetodoDoisConstString; const psParam1: string;
  const psParam2: string; const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
  overload;

procedure motelog(const poMetodo: TMoteLogMetodoUmString; const psParam1: string;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
procedure motelog(const poMetodo: TMoteLogMetodoUmConstStringUmConstBool;
  const psParam1: string; const pbParam2: boolean; const psNomeMetodo: string = '';
  const psTextoParaLog: string = ''); overload;


procedure motelog(const poMetodo: TMoteLogMetodoUmConstBool; const pbParam1: boolean;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
procedure motelog(const poMetodo: TMoteLogMetodoUmConstInt; const pnParam1: integer;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
procedure motelog(const poMetodo: TMoteLogMetodoUmConstObj; const poParam1: TObject;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
procedure motelog(const poMetodo: TMoteLogMetodoUmVariant; const pvParam1: variant;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;

implementation

uses
  inifiles, TypInfo;

const
  sSECAO_INI_LOG = 'LOG';

var
  goLog: TMoteLog;
  gbLeuLog: boolean;
  gsUsuariosParaLog: string;
  gnNivelIndentacaoLimite: integer;
  gsPastaLog: string;
  gbSalvarDataSets: boolean;
  gsNivelLogIni: string;
  gbAdicionarTempoMetodo: boolean;


function GeTMoteLog(const pvSpDB: olevariant; const psUsuarioLogado: string;
  const pbEstaNoServidor: boolean): TMoteLog; overload;
begin
  if not Assigned(goLog) then
  begin
    goLog := TMoteLog.Create(pvSpDB, psUsuarioLogado); //PC_OK
    if pbEstaNoServidor then
      goLog.prcDelocamentolIndetacaoNoArquivo := nDESCL_TEXTO_SERVIDOR_APP;
  end;

  goLog.RecarregarAtualizacoes;

  result := goLog;
end;

function GeTMoteLog(const pvSpDB: olevariant; const pbEstaNoServidor: boolean = False): TMoteLog;
  overload;
var
  sUsuarioLotado: string;
begin
  sUsuarioLotado := STRING_INDEFINIDO;
  result := GeTMoteLog(pvSpDB, sUsuarioLotado, pbEstaNoServidor);
end;


{ TMoteLog }

procedure TMoteLog.IniciarArquivoLog;
const
  sNOME_BASE = '%smotelog-%s-%s.txt';
var
  sPasta, sAgora: string;
begin
  if FsNomeArquivo = STRING_INDEFINIDO then
    FnQtdDatasetSalvos := 0;
  sAgora := FormatDatetime('yyyy-mm-dd', Now);
  sPasta := FsPastaLog;
  if (sPasta <> '') and (Copy(sPasta, Length(sPasta), 1) <> '\') then
    sPasta := sPasta + '\';

  FsNomeArquivo := Format(sNOME_BASE, [sPasta, sAgora, FsUsuarioLogado]);

  AssignFile(FoArquivoLog, FsNomeArquivo);
  if not FileExists(FsNomeArquivo) then
  begin
    try
      Rewrite(FoArquivoLog);
      Writeln(FoArquivoLog, 'Log gerado por TMoteLog');
    finally
      CloseFile(FoArquivoLog);
    end;
  end;
end;


constructor TMoteLog.Create(const pvSpDB: olevariant; const psUsuarioLogado: string);
begin
  FnNivelIndetacao := 0;
  FnDelocamentolIndetacaoNoArquivo := 0;
  FnNivelIndentacaoLimite := 1000;
  FenNivelLog := nlErro;
  FbLogAtivo := False;
  FvSpDB := pvSpDB;
  FsUsuarioLogado := psUsuarioLogado;

  LerIni;
  DefinirSeLogEstaAtivo;

  if FbLogAtivo then
    IniciarArquivoLog;
end;


destructor TMoteLog.Destroy;
begin
  FvSpDB := null;
  inherited;
end;

procedure TMoteLog.LerIni;
var
  oIni: TIniFile;

  s: string;
  nVal: integer;
  enNivelLog: TprcNivelLog;
begin
  if not gbLeuLog then
  begin
    gbLeuLog := True;
    oIni := TIniFile.Create(ParamStr(0)+'.ini');
    try
      gsUsuariosParaLog := oIni.ReadString(sSECAO_INI_LOG, 'UsuariosParaRegistrarLog',
        STRING_INDEFINIDO);
      gnNivelIndentacaoLimite := oIni.ReadInteger(sSECAO_INI_LOG, 'IndetacaoLimiteLog',
        FnNivelIndentacaoLimite);
      gsPastaLog := oIni.ReadString(sSECAO_INI_LOG, 'PastaLog', STRING_INDEFINIDO);
      gbSalvarDataSets := oIni.ReadString(sSECAO_INI_LOG, 'SalvarDataSetsLog', 'N') = 'S';
      gbAdicionarTempoMetodo := oIni.ReadString(sSECAO_INI_LOG, 'AdicionarTempoMetodo', 'S') = 'S';

      gsNivelLogIni := UpperCase(oIni.ReadString(sSECAO_INI_LOG, 'NivelLog', 'ERRO'));
    finally
      FreeAndNil(oIni);
    end;
  end;

  FsUsuariosParaLog := gsUsuariosParaLog;
  FnNivelIndentacaoLimite := gnNivelIndentacaoLimite;
  FsPastaLog := gsPastaLog;
  FbSalvarDataSets := gbSalvarDataSets;
  FbAdicionarTempoMetodo := gbAdicionarTempoMetodo;

  nVal := StrToIntDef(gsNivelLogIni, -1);
  if (nVal >= integer(low(TprcNivelLog))) and (nVal <= integer(High(TprcNivelLog))) then
  begin
    FenNivelLog := TprcNivelLog(nVal);
  end
  else
  begin
    for enNivelLog := low(TprcNivelLog) to High(TprcNivelLog) do
    begin
      s := UpperCase(GetEnumName(TypeInfo(TprcNivelLog), integer(enNivelLog)));

      if Pos(gsNivelLogIni, s) > 0 then
      begin
        FenNivelLog := enNivelLog;
        Break;
      end;
    end;
  end;
end;

procedure TMoteLog.DefinirSeLogEstaAtivo;
var
  slUsuarios: TStringList;
begin
  FbLogAtivo := False;
  if FsUsuariosParaLog = STRING_INDEFINIDO then
    exit;
  if FsUsuariosParaLog = '*' then
  begin
    FbLogAtivo := True;
    exit;
  end;

  slUsuarios := TStringList.Create;
  try
    slUsuarios.CommaText := FsUsuariosParaLog;
    FbLogAtivo := slUsuarios.IndexOf(FsUsuarioLogado) >= 0;
  finally
    FreeAndNil(slUsuarios);
  end;
end;

function TMoteLog.GerarLogAviso(const psTexto: string;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
begin
  result := GerarLog(psTexto, nlAviso, pbEntradaMetodo);
end;

function TMoteLog.GerarLogErro(const psTexto: string;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
begin
  result := GerarLog(psTexto, nlErro, pbEntradaMetodo);
end;

// 28/02/2019 - maxback -
function TMoteLog.GerarLogExcecao(const psTextoAdicional: string): IMoteContextoRegistroLog;
var
  oException: Exception;
  sMsgErro, sClasse: string;
begin
  //22/10/2019 - maxback - Story 220817 - Pega classe do erro também
  sMsgErro := EmptyStr;
  sClasse := EmptyStr;
  oException := Exception(ExceptObject);
  if Assigned(oException) then
  begin
    sMsgErro := oException.Message;
    sClasse := oException.ClassName;
  end;

  sMsgErro := 'Erro (' + sClasse + '): "' + sMsgErro + '"';

  if Length(psTextoAdicional) > 0 then
    sMsgErro := sMsgErro + ' (' + psTextoAdicional + ')';

  sMsgErro := sMsgErro + '.';

  result := GerarLog(sMsgErro, nlErro, False);
end;

function TMoteLog.CalcularTextoDifTempo(const poContexto: IMoteContextoRegistroLog): string;
const
  nTEMPOSREF: array[0..11] of double =
    (1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 10.0, 20.0, 30.0, (1.0 * 60.0), (2.0 * 60.0), (3.0 * 60.0));
var
  dtDif: TDateTime;
  nHour, nMin, nSec, nMSec: word;
  nDifAntMenor, nDifComp, nDifSegundos: double;
  i, j: integer;
  bAchou: boolean;
begin
  result := STRING_INDEFINIDO;
  if not FbAdicionarTempoMetodo then
    Exit;
  dtDif := now - poContexto.prcDataHoraCriacao;
  DecodeTime(dtDif, nHour, nMin, nSec, nMSec);

  nDifSegundos := (nHour * 60.0 * 60.0) + (nMin * 60.0) + (nSec * 1.0) + ((nMSec / 1000.0));

  result := Format('t=%0.2d:%0.2d:%0.2d.%0.3d (t=%.0fs) ', [nHour, nMin, nSec,
    nMSec, nDifSegundos]);

  bAchou := False;
  nDifAntMenor := 0.0;
  for i := Low(nTEMPOSREF) to High(nTEMPOSREF) do
  begin
    nDifComp := nTEMPOSREF[i];
    if nDifComp >= nDifSegundos then
    begin
      bAchou := True;
      result := result + Format('[t<%.0fs]', [nDifComp]);
      result := result + Format('[t>=%.0fs]', [nDifAntMenor]);
      for j := i - 2 downto Low(nTEMPOSREF) do
        result := result + Format('[t>=%.0fs]', [nTEMPOSREF[j]]);
      break;
    end;
    nDifAntMenor := nDifComp;
  end;
  if not bAchou then
  begin
    i := High(nTEMPOSREF);
    result := result + Format(' [t>=%.0fs]', [nTEMPOSREF[i]]);
  end;

  result := ' /* ' + result + ' */ ';
end;


function TMoteLog.GerarLogRetorno(const poContexto: IMoteContextoRegistroLog;
  const psRetorno: string; const psTexto: string): IMoteContextoRegistroLog;
var
  sAdicional: string;
begin
  try
    if poContexto.prcEntradaMetodo then
      FnNivelIndetacao := FnNivelIndetacao - 1;

    sAdicional := ' -> Retorno = "' + psRetorno + '"';
    if psTexto <> STRING_INDEFINIDO then
      sAdicional := sAdicional + ' (' + psTexto + ')';
    sAdicional := CalcularTextoDifTempo(poContexto) + sAdicional;
    result := GerarLog(poContexto.prcTexto + sAdicional, poContexto.prcNivel);
  except  //NOSONAR
    result := GerarLog('Erro tratando chamada ao log: GerarLogRetorno()', nlErro);
  end;
end;

//22/10/2019 - maxback - Story 220817 -
procedure TMoteLog.GerarLogRetornoMetodo(const psRetorno: string; const psTexto: string);
var
  sAdicional: string;
begin
  try
    FnNivelIndetacao := FnNivelIndetacao - 1;

    sAdicional := ' -> Retorno = "' + psRetorno + '"';
    if psTexto <> STRING_INDEFINIDO then
      sAdicional := sAdicional + ' (' + psTexto + ')';
    GerarLog(FsUltimoTextoInicioBloco + sAdicional, nlDebug);
  except  //NOSONAR
    GerarLog('Erro tratando chamada ao log: GerarLogRetorno()', nlErro);
  end;
end;

procedure TMoteLog.GerarLogRetornoMetodoComErro;
begin
  try
    GerarLogExcecao;
    GerarLogRetornoMetodo;
  except  //NOSONAR
    GerarLog('Erro tratando chamada ao log: GerarLogRetornoMetodoComErro()', nlErro);
  end;
end;


// 28/02/2019 - maxback -
function TMoteLog.GerarLogFmt(const psStringFormatacao: string;
  const poDadosParametrosFormat: array of const; const penNivelLog: TprcNivelLog;
  const pbEntradaMetodo: boolean): IMoteContextoRegistroLog;
var
  sTexto: string;
begin
  try
    sTexto := Format(psStringFormatacao, poDadosParametrosFormat);
  except //NOSONAR
    sTexto := 'Erro tratando string de formatação ao gerar log (' + psStringFormatacao + ')';
  end;

  result := GerarLog(sTexto, penNivelLog, pbEntradaMetodo);
end;

//15/02/2019 - maxback - Defect 178054 -
function TMoteLog.GerarLogRetornoFmt(const poContexto: IMoteContextoRegistroLog;
  const psRetorno: string; const psStringFormatacao: string;
  const poDadosParametrosFormat: array of const): IMoteContextoRegistroLog;
var
  sTexto: string;
begin
  try
    sTexto := Format(psStringFormatacao, poDadosParametrosFormat);
  except //NOSONAR
    sTexto := 'Erro tratando string de formatação ao gerar log (' + psStringFormatacao + ')';
  end;

  result := GerarLogRetorno(poContexto, psRetorno, sTexto);
end;

function TMoteLog.GerarLogRetornoBool(const poContexto: IMoteContextoRegistroLog;
  const pbRetorno: boolean; const psTexto: string): IMoteContextoRegistroLog;
const
  sBOOL: array[boolean] of string = ('False', 'True');
var
  sAdicional: string;
begin
  try
    if poContexto.prcEntradaMetodo then
      FnNivelIndetacao := FnNivelIndetacao - 1;

    sAdicional := ' -> Retorno = ' + sBOOL[pbRetorno];
    if psTexto <> STRING_INDEFINIDO then
      sAdicional := sAdicional + ' (' + psTexto + ')';
    sAdicional := CalcularTextoDifTempo(poContexto) + sAdicional;
    result := GerarLog(poContexto.prcTexto + sAdicional, poContexto.prcNivel);
  except //NOSONAR
    result := GerarLog('Erro tratando chamada ao log: GerarLogRetornoBool()', nlErro);
  end;
end;


function TMoteLog.GerarLog(const psTexto: string; const penNivelLog: TprcNivelLog;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
var
  sNivel, sIndentacao: string;
  nTentativas: integer;
begin
  try
    if not FbLogAtivo then
      exit;
    if penNivelLog > FenNivelLog then
      exit;

    if FnNivelIndetacao <= FnNivelIndentacaoLimite then
    begin
      sNivel := GetEnumName(TypeInfo(TprcNivelLog), integer(penNivelLog));
      sNivel := Format('%7s', [sNivel]);

      sIndentacao := StringOfChar(' ', FnDelocamentolIndetacaoNoArquivo) +
        StringOfChar(' ', FnNivelIndetacao * 2);

      try
        for nTentativas := 0 to 3 do
        begin
          try
            AssignFile(FoArquivoLog, FsNomeArquivo);
            if FileExists(FsNomeArquivo) then
              Append(FoArquivoLog)
            else
              Rewrite(FoArquivoLog);
            break;
          except
            if nTentativas = 3 then
            begin
              //desativa o log e sai
              gsUsuariosParaLog := '';
              FbLogAtivo := False;
              Exit;
            end;
          end;
        end;
        try
          Writeln(FoArquivoLog, '[' + FormatDatetime('dd/mm/yyyy - hh:mm:ss.zzz', now) +
            '][' + sNivel + '] - ' + sIndentacao + psTexto);
        finally
          CloseFile(FoArquivoLog);
        end;

        if (not VarIsEmpty(FvSpDB)) and (not VarIsNull(FvSpDB)) then
        begin
          if penNivelLog = nlErro then
            FvSpDB.LogAvisoErro('[' + sNivel + '] - ' + sIndentacao + psTexto)
          else
            FvSpDB.LogAviso('[' + sNivel + '] - ' + sIndentacao + psTexto);
        end;
      except //NOSONAR
        //mata erro
      end;
    end;
    if pbEntradaMetodo then
      FsUltimoTextoInicioBloco := psTexto;

    if pbEntradaMetodo then
      FnNivelIndetacao := FnNivelIndetacao + 1;
  finally
    result := TprlContextoRegistroLog.Create(psTexto, penNivelLog, pbEntradaMetodo);  //PC_OK
    result.motelog := self;
  end;
end;

function TMoteLog.GerarLogDebug(const psTexto: string;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
begin
  result := GerarLog(psTexto, nlDebug, pbEntradaMetodo);
end;

function TMoteLog.GerarLogInfo(const psTexto: string;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
begin
  result := GerarLog(psTexto, nlInfo, pbEntradaMetodo);
end;



procedure TMoteLog.RecarregarAtualizacoes;
begin
  LerIni;
  DefinirSeLogEstaAtivo;
  if FbLogAtivo then
    IniciarArquivoLog;
end;

{ TprlContextoRegistroLog }

constructor TprlContextoRegistroLog.Create(const psTexto: string;
  const penNivel: TprcNivelLog; const pbEntradaMetodo: boolean = False);
begin
  FsTexto := psTexto;
  FenNivel := penNivel;
  FbEntradaMetodo := pbEntradaMetodo;
  FdtDataHoraCriacao := now;
end;

function TprlContextoRegistroLog.GetEntradaMetodo: boolean;
begin
  result := FbEntradaMetodo;
end;

function TprlContextoRegistroLog.GetNivel: TprcNivelLog;
begin
  result := FenNivel;
end;

function TprlContextoRegistroLog.GetTexto: string;
begin
  result := FsTexto;
end;

procedure TprlContextoRegistroLog.SetEntradaMetodo(const Value: boolean);
begin
  FbEntradaMetodo := Value;
end;

procedure TprlContextoRegistroLog.SetNivel(const Value: TprcNivelLog);
begin
  FenNivel := Value;
end;

procedure TprlContextoRegistroLog.SetTexto(const Value: string);
begin
  FsTexto := Value;
end;

function TprlContextoRegistroLog.GetDataHoraCriacao: TDatetime;
begin
  result := FdtDataHoraCriacao;
end;

procedure TprlContextoRegistroLog.SetDataHoraCriacao(const Value: TDatetime);
begin
  FdtDataHoraCriacao := Value;
end;

function TprlContextoRegistroLog.GetLog: TMoteLog;
begin
  result := FoLog;
end;

procedure TprlContextoRegistroLog.SetLog(const Value: TMoteLog);
begin
  FoLog := Value;
end;

procedure TprlContextoRegistroLog.GerarLogRetornoMetodo(const psRetorno: string = 'void';
  const psTexto: string = STRING_INDEFINIDO);
begin
  if not Assigned(FoLog) then
    Exit;
  FoLog.GerarLogRetornoMetodo(psRetorno, psTexto);
end;

procedure TprlContextoRegistroLog.GerarLogRetornoMetodoComErro;
begin
  if not Assigned(FoLog) then
    Exit;
  FoLog.GerarLogRetornoMetodoComErro;
end;


function CriarContextoInicio(const poObj: TObject;
  const psNomeMetodo, psTextoParaLog: string): IMoteContextoRegistroLog;
var
  sTexto, sClasse: string;
  oLog: TMoteLog;
begin
  oLog := GeTMoteLog(null);
  sClasse := poObj.ClassName;
  sTexto := sClasse + '.' + psNomeMetodo;
  if psTextoParaLog <> EmptyStr then
    sTexto := sTexto + '   /* ' + psTextoParaLog + ' */';

  result := oLog.GerarLogDebug(sTexto, True);
end;


function motelog(const psNomeMetodoComClasse: string = ''; const psTextoParaLog: string = '';
  const pbInicioMetodo: boolean = False): IMoteContextoRegistroLog;
var
  oLog: TMoteLog;
  sTexto: string;
begin
  oLog := GeTMoteLog(null);
  sTexto := psNomeMetodoComClasse;
  if psTextoParaLog <> EmptyStr then
    sTexto := sTexto + '   /* ' + psTextoParaLog + ' */';
  result := oLog.GerarLogDebug(sTexto, pbInicioMetodo);
end;

function motelogfmt(const psStringFormatacao: string; const poDadosParametrosFormat: array of const;
  const penNivelLog: TprcNivelLog = nlDebug;
  const pbEntradaMetodo: boolean = False): IMoteContextoRegistroLog;
var
  oLog: TMoteLog;
begin
  oLog := GeTMoteLog(null);
  result := oLog.GerarLogFmt(psStringFormatacao, poDadosParametrosFormat,
    penNivelLog, pbEntradaMetodo);
end;


procedure motelogexception(const psTextoAdicional: string = STRING_INDEFINIDO);
var
  oLog: TMoteLog;
begin
  oLog := GeTMoteLog(null);
  oLog.GerarLogExcecao(psTextoAdicional);
end;

procedure motelog(const poMetodo: TMoteLogMetodoSemParametros; const psNomeMetodo: string = '';
  const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo;
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

procedure motelog(const poMetodo: TMoteLogMetodoUmString; const psParam1: string;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = ''); overload;
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(psParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;


procedure motelog(const poMetodo: TMoteLogMetodoUmConstString; const psParam1: string;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(psParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

procedure motelog(const poMetodo: TMoteLogMetodoDoisConstString; const psParam1: string;
  const psParam2: string; const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(psParam1, psParam2);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;


procedure motelog(const poMetodo: TMoteLogMetodoUmConstStringUmConstBool;
  const psParam1: string; const pbParam2: boolean; const psNomeMetodo: string = '';
  const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(psParam1, pbParam2);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

procedure motelog(const poMetodo: TMoteLogMetodoUmConstBool; const pbParam1: boolean;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(pbParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

procedure motelog(const poMetodo: TMoteLogMetodoUmConstInt; const pnParam1: integer;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(pnParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

procedure motelog(const poMetodo: TMoteLogMetodoUmConstObj; const poParam1: TObject;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(poParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;



procedure motelog(const poMetodo: TMoteLogMetodoUmVariant; const pvParam1: variant;
  const psNomeMetodo: string = ''; const psTextoParaLog: string = '');
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  oContextoLog := CriarContextoInicio(TObject(TMethod(poMetodo).Data),
    psNomeMetodo, psTextoParaLog);
  try
    try
      poMetodo(pvParam1);
    except
      oContextoLog.motelog.GerarLogExcecao;
      raise;
    end;
  finally
    oContextoLog.motelog.GerarLogRetorno(oContextoLog);
  end;
end;

initialization
  goLog := nil;
  gbLeuLog := False;
  gsUsuariosParaLog := '*';
  gnNivelIndentacaoLimite := 99;
  gsPastaLog := '.';
  gbSalvarDataSets := False;
  gsNivelLogIni := 'Debug';
  gbAdicionarTempoMetodo := False;

finalization
  FreeAndNil(goLog); //PC_OK

end.

