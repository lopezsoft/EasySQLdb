{
    Copyright (c) 2004-2014 by Joost van der Sluis, FPC contributors

    SQL database & dataset

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit easyDB;

{$mode objfpc}
{$H+}
{$M+}   // ### remove this!!!

interface

uses SysUtils, Classes, DB, bufdataset, sqlscript, sqltypes,
  ExtCtrls,xmldatapacketreader,
  FileUtil,Controls,typinfo,LazUTF8;

type
  TSchemaType = sqltypes.TSchemaType;
  TStatementType = sqltypes.TStatementType;
  TDBEventType = sqltypes.TDBEventType;
  TDBEventTypes = sqltypes.TDBEventTypes;
  TQuoteChars = sqltypes.TQuoteChars;
  {News}
  TQueryType  = (
    qtSingle,
    qtComposite
  );

const
  StatementTokens : Array[TStatementType] of string = ('(unknown)', 'select',
                  'insert', 'update', 'delete',
                  'create', 'get', 'put', 'execute',
                  'start','commit','rollback', '?'
                 );
  TSchemaObjectNames: array[TSchemaType] of String = ('???', 'table_name',
      '???', 'procedure_name', 'column_name', 'param_name',
      'index_name', 'package_name', 'schema_name','sequence');
  SingleQuotes : TQuoteChars = ('''','''');
  DoubleQuotes : TQuoteChars = ('"','"');
  LogAllEvents      = [detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack];
  LogAllEventsExtra = [detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack, detParamValue,detActualSQL];

  // Backwards compatibility alias constants.

  stNoSchema         = sqltypes.stNoSchema;
  stTables           = sqltypes.stTables;
  stSysTables        = sqltypes.stSysTables;
  stProcedures       = sqltypes.stProcedures;
  stColumns          = sqltypes.stColumns;
  stProcedureParams  = sqltypes.stProcedureParams;
  stIndexes          = sqltypes.stIndexes;
  stPackages         = sqltypes.stPackages;
  stSchemata         = sqltypes.stSchemata;
  stSequences        = sqltypes.stSequences;

  stUnknown       = sqltypes.stUnknown;
  stSelect        = sqltypes.stSelect;
  stInsert        = sqltypes.stInsert;
  stUpdate        = sqltypes.stUpdate;
  stDelete        = sqltypes.stDelete;
  stDDL           = sqltypes.stDDL;
  stGetSegment    = sqltypes.stGetSegment;
  stPutSegment    = sqltypes.stPutSegment;
  stExecProcedure = sqltypes.stExecProcedure;
  stStartTrans    = sqltypes.stStartTrans;
  stCommit        = sqltypes.stCommit;
  stRollback      = sqltypes.stRollback;
  stSelectForUpd  = sqltypes.stSelectForUpd;

  detCustom      = sqltypes.detCustom;
  detPrepare     = sqltypes.detPrepare;
  detExecute     = sqltypes.detExecute;
  detFetch       = sqltypes.detFetch;
  detCommit      = sqltypes.detCommit;
  detRollBack    = sqltypes.detRollBack;
  detParamValue  = sqltypes.detParamValue;
  detActualSQL   = sqltypes.detActualSQL;

Type
  TRowsCount = LargeInt;

  TSQLStatementInfo = Record
    StatementType : TStatementType;
    TableName     : String;
    Updateable    : Boolean;
    WhereStartPos ,
    WhereStopPos : integer;
    QueryType    : TQueryType
  end;

  TEDBConnection = class;
  TEDBTransaction = class;
  TCustomEDBQuery = class;
  TCustomSQLStatement = Class;
  TEDBQuery = class;
  TEDBScript = class;

  TSQLHandle = Class(TObject)
  end;

  { TSQLCursor }

  TSQLCursor = Class(TSQLHandle)
  public
    FPrepared      : Boolean;
    FSelectable    : Boolean;
    FInitFieldDef  : Boolean;
    FStatementType : TStatementType;
    FSchemaType    : TSchemaType;
    FQueryType     : TQueryType;
  end;

  { ESQLDatabaseError}

  ESQLDatabaseError = class(EDatabaseError)
    public
      ErrorCode: integer;
      SQLState : string;
      constructor CreateFmt(const Fmt: string; const Args: array of const;
                            Comp : TComponent; AErrorCode: integer; ASQLState: string); overload;
  end;

  { TSQLDBFieldDef }

  TSQLDBFieldDef = Class(TFieldDef)
  private
    FData: Pointer;
  Public
    Property SQLDBData : Pointer Read FData Write FData;
  end;

  { TSQLDBFieldDefs }

  TSQLDBFieldDefs = Class(TFieldDefs)
  Protected
    Class Function FieldDefClass : TFieldDefClass; override;
  end;

  { TSQLDBParam }

  TSQLDBParam = Class(TParam)
  private
    FFieldDef : TFieldDef;
    FData     : Pointer;
  Public
    Property FieldDef : TFieldDef Read FFieldDef Write FFieldDef;
    Property SQLDBData : Pointer Read FData Write FData;
  end;

  { TSQLDBParams }

  TSQLDBParams = Class(TParams)
  Protected
    Class Function ParamClass : TParamClass; override;
  end;


type

  { TServerIndexDefs }

  TServerIndexDefs = class(TIndexDefs)
  Private
  public
    constructor Create(ADataSet: TDataSet); override;
    procedure Update; override;
  end;

type

  { TEDBConnection }

  TDBLogNotifyEvent = Procedure (Sender : TEDBConnection; EventType : TDBEventType; Const Msg : String) of object;

  TConnOption = (sqSupportParams, sqSupportEmptyDatabaseName, sqEscapeSlash, sqEscapeRepeat, sqImplicitTransaction, sqLastInsertID, sqSupportReturning);
  TConnOptions= set of TConnOption;

  TSQLConnectionOption = (scoExplicitConnect, scoApplyUpdatesChecksRowsAffected);
  TSQLConnectionOptions = Set of TSQLConnectionOption;

  TConnInfoType=(citAll=-1, citServerType, citServerVersion, citServerVersionString, citClientName, citClientVersion);

  TEDBConnection = class (TDatabase)
  private
    FFieldNameQuoteChars : TQuoteChars;
    FOptions             : TSQLConnectionOptions;
    FPassword            : string;
    FPConnection         : Boolean;
    FTransaction         : TEDBTransaction;
    FUserName            : string;
    FHostName            : string;
    FCharSet             : string;
    FRole                : String;
    FStatements          : TFPList;
    FLogEvents           : TDBEventTypes;
    FOnLog               : TDBLogNotifyEvent;
    function GetPort     : cardinal;
    procedure SetOptions(AValue: TSQLConnectionOptions);
    procedure SetPConnection(AValue: Boolean);
    procedure SetPort(const AValue: cardinal);
    function AttemptCommit(trans : TSQLHandle) : boolean;
    function AttemptRollBack(trans : TSQLHandle) : boolean;
  protected
    FConnOptions         : TConnOptions;
    FSQLFormatSettings   : TFormatSettings;

    // La actualización de registros de base de datos se mueve fuera de TEDBQuery
    // Updating of DB records is moved out of TEDBQuery.
    // Se hace aquí, por lo que los descendientes pueden invalidarlo e implementar DB-specific.
    // It is done here, so descendents can override it and implement DB-specific.
    // Un día, esto puede ser factorizado a una clase TSQLResolver
    // Lo siguiente permite la construcción de consultas de actualización.
    // Pueden ser adaptados según sea necesario por los descendientes para adaptarse al motor DB
    // One day, this may be factored out to a TSQLResolver class.
    // The following allow construction of update queries.
    // They can be adapted as needed by descendents to fit the DB engine.

    procedure AddFieldToUpdateWherePart(var sql_where: string; UpdateMode : TUpdateMode; F: TField); virtual;
    function ConstructInsertSQL(Query: TCustomEDBQuery; Var ReturningClause : Boolean): string; virtual;
    function ConstructUpdateSQL(Query: TCustomEDBQuery; Var ReturningClause : Boolean): string; virtual;
    function ConstructDeleteSQL(Query: TCustomEDBQuery): string; virtual;
    function ConstructRefreshSQL(Query: TCustomEDBQuery; UpdateKind : TUpdateKind): string; virtual;
    function InitialiseUpdateStatement(Query: TCustomEDBQuery; var qry: TCustomEDBQuery): TCustomEDBQuery;
    function GetStructureTables(Query: TCustomEDBQuery; var qry: TCustomEDBQuery): TCustomEDBQuery;
    procedure ApplyFieldUpdate(C : TSQLCursor; P: TSQLDBParam; F: TField; UseOldValue: Boolean); virtual;
    // This is the call that updates a record, it used to be in TEDBQuery.
    // Esta es la llamada que actualiza un registro, solía estar en TEDBQuery
    procedure ApplyRecUpdate(Query : TCustomEDBQuery; UpdateKind : TUpdateKind); virtual;
    function RefreshLastInsertID(Query : TCustomEDBQuery; Field : TField): Boolean; virtual; abstract;
    procedure GetDBInfo(const ASchemaType : TSchemaType; const ASchemaObjectName, AReturnField : string; AList: TStrings);
    procedure SetTransaction(Value : TEDBTransaction); virtual;
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetAsSQLText(Field : TField) : string; overload; virtual;
    function GetAsSQLText(Param : TParam) : string; overload; virtual;
    function GetHandle : pointer; virtual;
    Function LogEvent(EventType : TDBEventType) : Boolean;
    Procedure LogParams(Const AParams : TParams); virtual;
    Procedure Log(EventType : TDBEventType; Const Msg : String); virtual;
    Procedure RegisterStatement(S : TCustomSQLStatement);
    Procedure UnRegisterStatement(S : TCustomSQLStatement);
    Function AllocateCursorHandle : TSQLCursor; virtual; abstract;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); virtual; abstract;
    function StrToStatementType(s : string) : TStatementType; virtual;
    procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TEDBTransaction;buf : string; AParams : TParams); virtual; abstract;
    procedure UnPrepareStatement(cursor : TSQLCursor); virtual; abstract;
    procedure Execute(cursor: TSQLCursor;atransaction:TEDBTransaction; AParams : TParams); virtual; abstract;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; virtual;
    function Fetch(cursor : TSQLCursor) : boolean; virtual; abstract;
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs); virtual; abstract;
    function LoadField(cursor : TSQLCursor; FieldDef : TFieldDef; buffer : pointer; out CreateBlob : boolean) : boolean; virtual; abstract;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TEDBTransaction); virtual; abstract;
    procedure FreeFldBuffers(cursor : TSQLCursor); virtual;

    Function AllocateTransactionHandle : TSQLHandle; virtual; abstract;
    function GetTransactionHandle(trans : TSQLHandle): pointer; virtual; abstract;
    function Commit(trans : TSQLHandle) : boolean; virtual; abstract;
    function RollBack(trans : TSQLHandle) : boolean; virtual; abstract;
    function StartImplicitTransaction(trans : TSQLHandle; aParams : string) : boolean; virtual;
    function StartDBTransaction(trans : TSQLHandle; aParams : string) : boolean; virtual; abstract;
    procedure CommitRetaining(trans : TSQLHandle); virtual; abstract;
    procedure RollBackRetaining(trans : TSQLHandle); virtual; abstract;

    procedure UpdateIndexDefs(IndexDefs : TIndexDefs; TableName : string); virtual;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; virtual;
    function GetNextValueSQL(const SequenceName: string; IncrementBy: Integer): string; virtual;

    Procedure MaybeConnect;

    Property Statements : TFPList Read FStatements;
    property Port: cardinal read GetPort write SetPort;
  public
    property Handle: Pointer read GetHandle;
    property FieldNameQuoteChars: TQuoteChars read FFieldNameQuoteChars write FFieldNameQuoteChars;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure ExecuteDirect(SQL : String); overload; virtual;
    procedure ExecuteDirect(SQL : String; ATransaction : TEDBTransaction); overload; virtual;
    // Unified version
    function GetObjectNames(ASchemaType: TSchemaType; AList : TSqlObjectIdentifierList): Integer; virtual;
    // Older versions.
    procedure GetTableNames(List : TStrings; SystemTables : Boolean = false); virtual;
    procedure GetProcedureNames(List : TStrings); virtual;
    procedure GetFieldNames(const TableName : string; List : TStrings); virtual;
    procedure GetSchemaNames(List: TStrings); virtual;
    procedure GetSequenceNames(List: TStrings); virtual;
    function GetConnectionInfo(InfoType:TConnInfoType): string; virtual;
    function GetStatementInfo(const ASQL: string): TSQLStatementInfo; virtual;
    procedure CreateDB; virtual;
    procedure DropDB; virtual;
    function GetNextValue(const SequenceName: string; IncrementBy: integer=1): Int64; virtual;
    property ConnOptions: TConnOptions read FConnOptions;
    procedure CheckConnection;virtual;
    function GetLastInsertId : Integer;virtual; abstract;
  published
    property Password : string read FPassword write FPassword;
    property Transaction : TEDBTransaction read FTransaction write SetTransaction;
    property UserName : string read FUserName write FUserName;
    property CharSet : string read FCharSet write FCharSet;
    property HostName : string Read FHostName Write FHostName;
    Property OnLog : TDBLogNotifyEvent Read FOnLog Write FOnLog;
    Property LogEvents : TDBEventTypes Read FLogEvents Write FLogEvents Default LogAllEvents;
    Property Options : TSQLConnectionOptions Read FOptions Write SetOptions;
    Property Role :  String read FRole write FRole;
    property Connected;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

  { TEDBTransaction }

  TCommitRollbackAction = (caNone, caCommit, caCommitRetaining, caRollback,
    caRollbackRetaining);

  TSQLTransactionOption = (stoUseImplicit, stoExplicitStart);
  TSQLTransactionOptions = Set of TSQLTransactionOption;

  TEDBTransaction = class (TDBTransaction)
  private
    FOptions             : TSQLTransactionOptions;
    FTrans               : TSQLHandle;
    FAction              : TCommitRollbackAction;
    FParams              : TStringList;
    function GetSQLConnection: TEDBConnection;
    procedure SetOptions(AValue: TSQLTransactionOptions);
    procedure SetParams(const AValue: TStringList);
    procedure SetSQLConnection(AValue: TEDBConnection);
  protected
    Procedure MaybeStartTransaction;
    Function AllowClose(DS: TDBDataset): Boolean; override;
    function GetHandle : Pointer; virtual;
    Procedure SetDatabase (Value : TDatabase); override;
    Function LogEvent(EventType : TDBEventType) : Boolean;
    Procedure Log(EventType : TDBEventType; Const Msg : String); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    property Handle: Pointer read GetHandle;
    Property SQLConnection : TEDBConnection Read GetSQLConnection Write SetSQLConnection;
  published
    property Action : TCommitRollbackAction read FAction write FAction Default caRollBack;
    property Database;
    property Params : TStringList read FParams write SetParams;
    Property Options : TSQLTransactionOptions Read FOptions Write SetOptions;
  end;



  { TCustomSQLStatement }

  TCustomSQLStatement = Class(TComponent)
  Private
    FCursor           : TSQLCursor;
    FDatabase         : TEDBConnection;
    FParamCheck       : Boolean;
    FParams           : TParams;
    FSQL              : TStrings;
    FOrigSQL          : String;
    FServerSQL        : String;
    FTransaction      : TEDBTransaction;
    FParseSQL         : Boolean;
    FDataLink         : TDataLink;
    FRowsAffected     : TRowsCount;
    procedure SetCursor(AValue: TSQLCursor);
    procedure SetDatabase(AValue: TEDBConnection);
    procedure SetParams(AValue: TParams);
    procedure SetSQL(AValue: TStrings);
    procedure SetTransaction(AValue: TEDBTransaction);
    Function GetPrepared : Boolean;
  Protected
    Function CreateDataLink : TDataLink; virtual;
    procedure OnChangeSQL(Sender : TObject); virtual;
    function GetDataSource: TDataSource; Virtual;
    procedure SetDataSource(AValue: TDataSource); virtual;
    Procedure CopyParamsFromMaster(CopyBound : Boolean); virtual;
    procedure AllocateCursor;
    procedure DeAllocateCursor;
    Function GetSchemaType : TSchemaType; virtual;
    Function GetSchemaObjectName : String; virtual;
    Function GetSchemaPattern: String; virtual;
    Function IsSelectable : Boolean ; virtual;
    procedure GetStatementInfo(var ASQL: String; out Info: TSQLStatementInfo); virtual;
    Procedure DoExecute; virtual;
    procedure DoPrepare; virtual;
    procedure DoUnPrepare; virtual;
    Function CreateParams : TSQLDBParams; virtual;
    Function LogEvent(EventType : TDBEventType) : Boolean;
    Procedure Log(EventType : TDBEventType; Const Msg : String); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property Cursor : TSQLCursor read FCursor write SetCursor;
    Property Database : TEDBConnection Read FDatabase Write SetDatabase;
    Property Transaction : TEDBTransaction Read FTransaction Write SetTransaction;
    Property SQL : TStrings Read FSQL Write SetSQL;
    Property Params : TParams Read FParams Write SetParams;
    Property DataSource : TDataSource Read GetDataSource Write SetDataSource;
    Property ParseSQL : Boolean Read FParseSQL Write FParseSQL;
    Property ParamCheck : Boolean Read FParamCheck Write FParamCheck default true;
  Public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    Procedure Prepare;
    Procedure Execute;
    Procedure Unprepare;
    function ParamByName(Const AParamName : String) : TParam;
    function RowsAffected: TRowsCount; virtual;
    Property Prepared : boolean read GetPrepared;
  end;

  TSQLStatement = Class(TCustomSQLStatement)
  Published
    Property Database;
    Property DataSource;
    Property ParamCheck;
    Property Params;
    Property ParseSQL;
    Property SQL;
    Property Transaction;
  end;


  { TSQLSequence }

  TSQLSequenceApplyEvent = (saeOnNewRecord, saeOnPost);

  TSQLSequence = class(TPersistent)
  private
    FQuery     : TCustomEDBQuery;
    FFieldName : String;
    FSequenceName : String;
    FIncrementBy  : Integer;
    FApplyEvent   : TSQLSequenceApplyEvent;
  public
    constructor Create(AQuery: TCustomEDBQuery);
    procedure Assign(Source: TPersistent); override;
    procedure Apply;
    function GetNextValue: Int64;
  published
    property FieldName: String read FFieldName write FFieldName;
    property SequenceName: String read FSequenceName write FSequenceName;
    property IncrementBy: Integer read FIncrementBy write FIncrementBy default 1;
    property ApplyEvent: TSQLSequenceApplyEvent read FApplyEvent write FApplyEvent default saeOnNewRecord;
  end;


  { TCustomEDBQuery }

  TSQLQueryOption = (sqoKeepOpenOnCommit, sqoAutoApplyUpdates, sqoAutoCommit, sqoCancelUpdatesOnRefresh, sqoRefreshUsingSelect);
  TSQLQueryOptions = Set of TSQLQueryOption;

  TCustomEDBQuery = class (TCustomBufDataset)
  private
    FACloseConnection: Boolean;
    FEDatabase           : TDatabase;
    FETransaction        : TDBTransaction;
    FForceEditing        : Boolean;
    FOptions             : TSQLQueryOptions;
    FSchemaType          : TSchemaType;
    FUpdateable          : boolean;
    FTableName           : string;
    FStatement           : TCustomSQLStatement;
    FInsertSQL,
    FUpdateSQL,
    FDeleteSQL,
    FRefreshSQL          : TStringList;
    FUpdateCompositeQuery: Boolean;
    FIsEOF               : boolean;
    FLoadingFieldDefs    : boolean;
    FUpdateMode          : TUpdateMode;
    FusePrimaryKeyAsKey  : Boolean;
    FWhereStartPos       : integer;
    FWhereStopPos        : integer;
    FServerFilterText    : string;
    FServerFiltered      : Boolean;

    FServerIndexDefs     : TServerIndexDefs;

    // Used by SetSchemaType
    FSchemaObjectName    : string;
    FSchemaPattern       : string;

    FInsertQry,
    FUpdateQry,
    FDeleteQry           : TCustomEDBQuery;
    FSequence            : TSQLSequence;
    procedure FreeFldBuffers; virtual;
    function GetParamCheck: Boolean;
    function GetParams: TParams;
    function GetParseSQL: Boolean;
    function GetQueryType: TQueryType;
    function GetServerIndexDefs: TServerIndexDefs;
    function GetSQL: TStringList;
    function GetSQLConnection: TEDBConnection;
    function GetSQLTransaction: TEDBTransaction;
    function GetStatementType : TStatementType;
    Function NeedLastInsertID: TField;
    procedure SetACloseConnection(AValue: Boolean);
    procedure SetEDatabase(AValue: TDatabase);
    procedure SetETransaction(AValue: TDBTransaction);
    procedure SetForceEditing(AValue: Boolean);
    procedure SetOptions(AValue: TSQLQueryOptions);
    procedure SetParamCheck(AValue: Boolean);
    procedure SetSQLConnection(AValue: TEDBConnection);
    procedure SetSQLTransaction(AValue: TEDBTransaction);
    procedure SetInsertSQL(const AValue: TStringList);
    procedure SetUpdateCompositeQuery(AValue: Boolean);
    procedure SetUpdateSQL(const AValue: TStringList);
    procedure SetDeleteSQL(const AValue: TStringList);
    procedure SetRefreshSQL(const AValue: TStringList);
    procedure SetParams(AValue: TParams);
    procedure SetParseSQL(AValue : Boolean);
    procedure SetSQL(const AValue: TStringList);
    procedure SetUsePrimaryKeyAsKey(AValue : Boolean);
    procedure SetUpdateMode(AValue : TUpdateMode);
    procedure OnChangeModifySQL(Sender : TObject);
    procedure Execute;
    procedure ApplyFilter;
    Function AddFilter(SQLstr : string) : string;
  protected
    Function RefreshLastInsertID(Field: TField): Boolean; virtual;
    Function NeedRefreshRecord (UpdateKind: TUpdateKind): Boolean; virtual;
    Function RefreshRecord (UpdateKind: TUpdateKind) : Boolean; virtual;
    Procedure ApplyReturningResult(Q : TCustomEDBQuery; UpdateKind : TUpdateKind);
    Function Cursor : TSQLCursor;
    Function LogEvent(EventType : TDBEventType) : Boolean;
    Procedure Log(EventType : TDBEventType; Const Msg : String); virtual;
    // abstract & virtual methods of TBufDataset
    function Fetch : boolean; override;
    function LoadField(FieldDef : TFieldDef; buffer : pointer; out CreateBlob : boolean) : boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField); override;
    procedure ApplyRecUpdate(UpdateKind : TUpdateKind); override;
    procedure SetPacketRecords(aValue : integer); override;
    // abstract & virtual methods of TDataset
    procedure UpdateServerIndexDefs; virtual;
   // procedure SetDatabase(Value : TDatabase); override;
   // Procedure SetTransaction(Value : TDBTransaction); override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    Procedure InternalRefresh; override;
    function  GetCanModify: Boolean; override;
    Function IsPrepared : Boolean; virtual;
    Procedure SetActive (Value : Boolean); override;
    procedure SetServerFiltered(Value: Boolean); virtual;
    procedure SetServerFilterText(const Value: string); virtual;
    Function GetDataSource : TDataSource; override;
    Procedure SetDataSource(AValue : TDataSource);
    procedure BeforeRefreshOpenCursor; override;
    procedure SetReadOnly(AValue : Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoOnNewRecord; override;
    procedure DoBeforePost; override;
    class function FieldDefsClass : TFieldDefsClass; override;
    // IProviderSupport methods
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSGetTableName: string; override;
    Property TableName : String Read FTableName Write FTableName; // alternative: procedure DoGetTableName
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure ExecSQL; virtual;
    procedure SetSchemaInfo( ASchemaType : TSchemaType; ASchemaObjectName, ASchemaPattern : string); virtual;
    function RowsAffected: TRowsCount; virtual;
    function ParamByName(Const AParamName : String) : TParam;
    Property Prepared : boolean read IsPrepared;
    Property SQLConnection : TEDBConnection Read GetSQLConnection Write SetSQLConnection;
    Property SQLTransaction: TEDBTransaction Read GetSQLTransaction Write SetSQLTransaction;
    property QueryType : TQueryType Read GetQueryType;
    // overriden TBufDataSet methods
    Procedure ApplyUpdates(MaxErrors: Integer); override; overload;
    // overriden TDataSet methods
    Procedure Post; override;
    Procedure Delete; override;
    property EDatabase : TDatabase read FEDatabase write SetEDatabase;
    property ETransaction : TDBTransaction read FETransaction write SetETransaction;
     //Función de ordenación usando RTTI.
    function SortDataSet(DataSet: TCustomEDBQuery;const FieldName: String): Boolean;
    // News
    property UpdateCompositeQuery : Boolean read FUpdateCompositeQuery write SetUpdateCompositeQuery;
    property ACloseConnection : Boolean read FACloseConnection write SetACloseConnection;
  protected
    // redeclared TDataSet properties
    property Active;
    property Filter;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AutoCalcFields;
    // protected
   // property Database;
   // property Transaction;
    property SchemaType : TSchemaType read FSchemaType default stNoSchema;
    property SQL : TStringlist read GetSQL write SetSQL;
    property InsertSQL : TStringList read FInsertSQL write SetInsertSQL;
    property UpdateSQL : TStringList read FUpdateSQL write SetUpdateSQL;
    property DeleteSQL : TStringList read FDeleteSQL write SetDeleteSQL;
    property RefreshSQL : TStringList read FRefreshSQL write SetRefreshSQL;
    Property Options : TSQLQueryOptions Read FOptions Write SetOptions;
    property Params : TParams read GetParams Write SetParams;
    Property ParamCheck : Boolean Read GetParamCheck Write SetParamCheck default true;
    property ParseSQL : Boolean read GetParseSQL write SetParseSQL default true;
    property UpdateMode : TUpdateMode read FUpdateMode write SetUpdateMode default upWhereKeyOnly;
    property UsePrimaryKeyAsKey : boolean read FUsePrimaryKeyAsKey write SetUsePrimaryKeyAsKey default true;
    property StatementType : TStatementType read GetStatementType;
    Property DataSource : TDataSource Read GetDataSource Write SetDataSource;
    property Sequence: TSQLSequence read FSequence write FSequence;
    property ServerFilter: string read FServerFilterText write SetServerFilterText;
    property ServerFiltered: Boolean read FServerFiltered write SetServerFiltered default False;
    property ServerIndexDefs : TServerIndexDefs read GetServerIndexDefs;
    property ForceEditing    : Boolean read FForceEditing write SetForceEditing;
  end;

{ TEDBQuery }
  TEDBQuery = Class(TCustomEDBQuery)
  public
    property SchemaType;
    Property StatementType;
  Published
    //property TableName;
    //property AutoClearFields;
    property MaxIndexesCount;
   // TDataset stuff
    property FieldDefs;
    Property Active;
    Property AutoCalcFields;
    Property Filter;
    Property Filtered;
    Property AfterCancel;
    Property AfterClose;
    Property AfterDelete;
    Property AfterEdit;
    Property AfterInsert;
    Property AfterOpen;
    Property AfterPost;
    Property AfterScroll;
    Property BeforeCancel;
    Property BeforeClose;
    Property BeforeDelete;
    Property BeforeEdit;
    Property BeforeInsert;
    Property BeforeOpen;
    Property BeforePost;
    Property BeforeScroll;
    Property OnCalcFields;
    Property OnDeleteError;
    Property OnEditError;
    Property OnFilterRecord;
    Property OnNewRecord;
    Property OnPostError;

    //    property SchemaInfo default stNoSchema;
   // property Database;
   // property Transaction;
    property EDatabase;
    property ETransaction;
    property ReadOnly;
    property SQL;
    property InsertSQL;
    property UpdateSQL;
    property DeleteSQL;
    property RefreshSQL;
    property IndexDefs;
    Property Options;
    property Params;
    Property ParamCheck;
    property ParseSQL;
    property UpdateMode;
    property UsePrimaryKeyAsKey;
    Property DataSource;
    property Sequence;
    property ServerFilter;
    property ServerFiltered;
    property ServerIndexDefs;
    property UpdateCompositeQuery;
    property ACloseConnection;
    property ForceEditing;
  end;

{ TEDBScript }

  TEDBScript = class (TCustomSQLscript)
  private
    FOnDirective: TSQLScriptDirectiveEvent;
    FQuery   : TCustomEDBQuery;
    FDatabase : TDatabase;
    FTransaction : TDBTransaction;
  protected
    procedure ExecuteStatement (SQLStatement: TStrings; var StopExecution: Boolean); override;
    procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); override;
    procedure ExecuteCommit(CommitRetaining: boolean=true); override;
    Procedure SetDatabase (Value : TDatabase); virtual;
    Procedure SetTransaction(Value : TDBTransaction); virtual;
    Procedure CheckDatabase;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ExecuteScript;
  published
    Property DataBase : TDatabase Read FDatabase Write SetDatabase;
    Property Transaction : TDBTransaction Read FTransaction Write SetTransaction;
    property OnDirective: TSQLScriptDirectiveEvent read FOnDirective write FOnDirective;
    Property UseDollarString;
    Property DollarStrings;
    property Directives;
    property Defines;
    property Script;
    property Terminator;
    property CommentsinSQL;
    property UseSetTerm;
    property UseCommit;
    property UseDefines;
    property OnException;
  end;

  { TEDBConnector }

  TEDBConnector = Class(TEDBConnection)
  private
    FProxy          : TEDBConnection;
    FConnectorType  : String;
    procedure SetConnectorType(const AValue: String);
  protected
    procedure SetTransaction(Value : TEDBTransaction);override;
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    Procedure CheckProxy;
    Procedure CreateProxy; virtual;
    Procedure FreeProxy; virtual;
    function StrToStatementType(s : string) : TStatementType; override;
    function GetAsSQLText(Field : TField) : string; overload; override;
    function GetAsSQLText(Param : TParam) : string; overload; override;
    function GetHandle : pointer; override;
    function RefreshLastInsertID(Query: TCustomEDBQuery; Field: TField): Boolean;
      override;
    Function AllocateCursorHandle : TSQLCursor; override;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TEDBTransaction;buf : string; AParams : TParams); override;
    procedure Execute(cursor: TSQLCursor;atransaction:TEDBTransaction; AParams : TParams); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    function Fetch(cursor : TSQLCursor) : boolean; override;
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TfieldDefs); override;
    procedure UnPrepareStatement(cursor : TSQLCursor); override;
    function LoadField(cursor : TSQLCursor; FieldDef : TFieldDef; buffer : pointer; out CreateBlob : boolean) : boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TEDBTransaction); override;
    procedure FreeFldBuffers(cursor : TSQLCursor); override;

    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartDBTransaction(trans : TSQLHandle; aParams : string) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs; TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    Property Proxy : TEDBConnection Read FProxy;
  public
    function GetLastInsertId: Integer; override;
  Published
    Property ConnectorType : String Read FConnectorType Write SetConnectorType;
    property Port;
  end;

  TSQLConnectionClass = Class of TEDBConnection;

  { TConnectionDef }
  TLibraryLoadFunction = Function (Const S : AnsiString) : Integer;
  TLibraryUnLoadFunction = Procedure;
  TConnectionDef = Class(TPersistent)
    Class Function TypeName : String; virtual;
    Class Function ConnectionClass : TSQLConnectionClass; virtual;
    Class Function Description : String; virtual;
    Class Function DefaultLibraryName : String; virtual;
    Class Function LoadFunction : TLibraryLoadFunction; virtual;
    Class Function UnLoadFunction : TLibraryUnLoadFunction; virtual;
    Class Function LoadedLibraryName : string; virtual;
    Procedure ApplyParams(Params : TStrings; AConnection : TEDBConnection); virtual;
  end;
  TConnectionDefClass = class of TConnectionDef;

Var
  GlobalDBLogHook : TDBLogNotifyEvent;

Procedure RegisterConnection(Def : TConnectionDefClass);
Procedure UnRegisterConnection(Def : TConnectionDefClass);
Procedure UnRegisterConnection(ConnectionName : String);
Function GetConnectionDef(ConnectorName : String) : TConnectionDef;
Procedure GetConnectionList(List : TSTrings);

const DefaultSQLFormatSettings : TFormatSettings = (
  CurrencyFormat: 1;
  NegCurrFormat: 5;
  ThousandSeparator: #0;
  DecimalSeparator: '.';
  CurrencyDecimals: 2;
  DateSeparator: '-';
  TimeSeparator: ':';
  ListSeparator: ' ';
  CurrencyString: '$';
  ShortDateFormat: 'yyyy-mm-dd';
  LongDateFormat: '';
  TimeAMString: '';
  TimePMString: '';
  ShortTimeFormat: 'hh:nn:ss';
  LongTimeFormat: 'hh:nn:ss.zzz';
  ShortMonthNames: ('','','','','','','','','','','','');
  LongMonthNames: ('','','','','','','','','','','','');
  ShortDayNames: ('','','','','','','');
  LongDayNames: ('','','','','','','');
  TwoDigitYearCenturyWindow: 50;
);

implementation
uses dbconst, strutils;

Const
  // Flags to check which fields must be refreshed.
  RefreshFlags : Array [ukModify..ukInsert] of TProviderFlag = (pfRefreshOnUpdate,pfRefreshOnInsert);


function TimeIntervalToString(Time: TDateTime): string;
var
  millisecond: word;
  second     : word;
  minute     : word;
  hour       : word;
begin
  DecodeTime(Time,hour,minute,second,millisecond);
  hour := hour + trunc(Time)*24;
  Result := Format('%.2d:%.2d:%.2d.%.3d',[hour,minute,second,millisecond]);
end;

{$I easysqldb.inc}
Initialization

Finalization
  DoneDefs;
end.
