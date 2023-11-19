{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EASYDBLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  easyDB, EASYPQTEventMonitor, easysqldblib, EDBStringsPropertyEditorDlg, 
  registereasysqldb, easymysql40conn, easymysql41conn, easymysql50conn, 
  easymysql51conn, easymysql55conn, easymysql56conn, easymysql57conn, 
  EasySqlite3Conn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registereasysqldb', @registereasysqldb.Register);
end;

initialization
  RegisterPackage('EASYDBLaz', @Register);
end.
