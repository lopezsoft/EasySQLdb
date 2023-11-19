{
    This file is part of the Free Pascal Classes Library (FCL).
    Copyright (c) 2006-2014 by the Free Pascal development team

    SQLite3 connection for SQLDB

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}

{
  Based on an implementation by Martin Schreiber, part of MSEIDE.
  Reworked all code so it conforms to FCL coding standards.

  TSQLite3Connection properties
      Params - "foreign_keys=ON" - enable foreign key support for this connection:
                                   http://www.sqlite.org/foreignkeys.html#fk_enable

}

unit EasySqlite3Conn;

{$mode objfpc}{$H+}

interface

uses
  classes, db, bufdataset, easyDB, sqlite3dyn, types;

const
  sqliteerrormax = 99;

type
  PDateTime = ^TDateTime;

  TStringArray = Array of string;
  PStringArray = ^TStringArray;

  TArrayStringArray = Array of TStringArray;
  PArrayStringArray = ^TArrayStringArray;

  { TSQLite3Connection }

  TEasySQLite3Connection = class(TEDBConnection)
  private
    fhandle: psqlite3;
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLCursor; override;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    function StrToStatementType(s : string) : TStatementType; override;
    procedure PrepareStatement(cursor: TSQLCursor; ATransaction : TEDBTransaction; buf: string; AParams : TParams); override;
    procedure Execute(cursor: TSQLCursor;atransaction:TEDBTransaction; AParams : TParams); override;
    function Fetch(cursor : TSQLCursor) : boolean; override;
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs : TFieldDefs); override;
    procedure UnPrepareStatement(cursor : TSQLCursor); override;

    procedure FreeFldBuffers(cursor : TSQLCursor); override;
    function LoadField(cursor : TSQLCursor; FieldDef : TFieldDef; buffer : pointer; out CreateBlob : boolean) : boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TEDBTransaction); override;

    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartDBTransaction(trans : TSQLHandle; aParams : string) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;

    procedure UpdateIndexDefs(IndexDefs : TIndexDefs; TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    function RefreshLastInsertID(Query : TCustomEDBQuery; Field : TField): Boolean; override;
    // New methods
    procedure checkerror(const aerror: integer);
    function stringsquery(const asql: string): TArrayStringArray;
    procedure execsql(const asql: string);
  public
    constructor Create(AOwner : TComponent); override;
    procedure GetFieldNames(const TableName : string; List :  TStrings); override;
    function GetConnectionInfo(InfoType:TConnInfoType): string; override;
    procedure CreateDB; override;
    procedure DropDB; override;
    function GetInsertID: int64;
    // See http://www.sqlite.org/c3ref/create_collation.html for detailed information
    // If eTextRep=0 a default UTF-8 compare function is used (UTF8CompareCallback)
    // Warning: UTF8CompareCallback needs a wide string manager on Linux such as cwstring
    // Warning: CollationName has to be a UTF-8 string
    procedure CreateCollation(const CollationName: string; eTextRep: integer; Arg: Pointer=nil; Compare: xCompare=nil);
    procedure LoadExtension(LibraryFile: string);
  published
    property ConnOptions;
  end;

  { TEasySQLite3ConnectionDef }

  TEasySQLite3ConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
    class Function DefaultLibraryName : String; override;
    class Function LoadFunction : TLibraryLoadFunction; override;
    class Function UnLoadFunction : TLibraryUnLoadFunction; override;
    class function LoadedLibraryName: string; override;
  end;

Var
  SQLiteLibraryName : String absolute sqlite3dyn.SQLiteDefaultLibrary deprecated 'use sqlite3dyn.SQLiteDefaultLibrary instead';

implementation

uses
  dbconst, sysutils, dateutils, FmtBCD;

{$IF NOT DECLARED(JulianEpoch)} // sysutils/datih.inc
const
  JulianEpoch = TDateTime(-2415018.5); // "julian day 0" is January 1, 4713 BC 12:00AM
{$ENDIF}

type

 TStorageType = (stNone,stInteger,stFloat,stText,stBlob,stNull);

 TSQLite3Cursor = class(tsqlcursor)
  private
   fhandle : psqlite3;
   fconnection: TEasySQLite3Connection;
   fstatement: psqlite3_stmt;
   ftail: pchar;
   fstate: integer;
   fparambinding: array of Integer;
   procedure checkerror(const aerror: integer);
   procedure bindparams(AParams : TParams);
   Procedure Prepare(Buf : String; AParams : TParams);
   Procedure UnPrepare;
   Procedure Execute;
   Function Fetch : Boolean;
 public
   RowsAffected : Largeint;
 end;
 {$I easysqlite3conn.inc}

initialization
  RegisterConnection(TEasySQLite3ConnectionDef);

finalization
  UnRegisterConnection(TEasySQLite3ConnectionDef);

end.

