**Free

//
// IBMiNagios
//
// Created by: github.com/TickMogne, 2022.05.31
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Nagios) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-S RequestData Pointer;

Dcl-Proc Nagios;
  Dcl-S Cmd Char(3);

  // Load the request data
  RequestData = HttpRequestLoad();
  If (RequestData = *Null);
    // Header with Status Error
    Header('500');
    Return;
  Else;
    // Header with Status Ok
    Header('200');  
    
    // Check the configuration
    If (CheckConfig() = *Off);
      HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
      HttpResponse('Info=Auth error' + CHAR_CR + CHAR_LF);
      Return;
    EndIf;

    // Check the command
    Cmd = HttpRequestValue(RequestData: 'cmd');

    // Process the command
    Select;
      When (Cmd = '001'); // System information
        Cmd001();
      When (Cmd = '002'); // Output queue information
        Cmd002();
      When (Cmd = '003'); // List of active jobs
        Cmd003();
      When (Cmd = '004'); // SQL Statement
        Cmd004();
      When (Cmd = '005'); // List of active subsystems
        Cmd005();
      When (Cmd = '006'); // List of messages
        Cmd006();
      Other;
        HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
        HttpResponse('Info=Bad command' + CHAR_CR + CHAR_LF);
    EndSl;
  EndIf;
End-Proc;

Dcl-Proc CheckConfig;
  Dcl-Pi CheckConfig Ind;
  End-Pi;
  Dcl-S ConfigFile Char(128) Inz(*Blanks);
  Dcl-S File_io Pointer;
  Dcl-S Line Char(2000);
  Dcl-S Ret Pointer;
  Dcl-S i Int(10);
  Dcl-S Key Char(128);
  Dcl-S Value Char(128);
  Dcl-S AuthKey Char(32);
  Dcl-S AuthKeyOk Ind Inz(*Off);

  // Get the config file name from the environment value
  ConfigFile = HttpGetEnv('NAGIOS_CONFIG_FILE');
  If (ConfigFile = *Blanks);
    // If the environment value is not set, take the default config file name
    ConfigFile = '/etc/ibminagios.conf';
  EndIf;

  // Open the config file
  File_io = fopen(%Trim(ConfigFile) + x'00': 'r');
  If (File_io = *Null); // Check error
    Return *Off;
  EndIf;

  // Get the auth key
  AuthKey = HttpRequestValue(RequestData: 'AUTHKEY');

  // Read until EOF
  Dow (feof(File_io) = 0);
    // Read the line
    Ret = fgets(%Addr(Line): %Size(Line): File_io);
    If (Ret <> *Null);
      // Remove the trailing special characters
      i = strlen(%Addr(Line));
      Dow ((i > 0) And ((%Subst(Line:i:1) = CHAR_CR) Or (%Subst(Line:i:1) = CHAR_LF) Or (%Subst(Line:i:1) = CHAR_NL) Or (%Subst(Line:i:1) = *blank)));
        %Subst(Line:i:1) = x'00';
        i -= 1;
      EndDo;
      // Split the line into key and value
      Key = *Blanks;
      i = 1;
      Dow ((%Subst(Line: i: 1) <> x'00') And (%Subst(Line: i: 1) <> '='));
        Key = %Trim(Key) + %Subst(Line: i: 1);
        i += 1;
      EndDo;
      Value = *Blanks;
      If (%Subst(Line: i: 1) = '=');
        i += 1;
        Dow (%Subst(Line: i: 1) <> x'00');
          Value = %Trim(Value) + %Subst(Line: i: 1);
          i += 1;
        EndDo;
      EndIf;
      // Check auth key
      If (%Trim(Key) = 'AUTHKEY');
        If (%Trim(Value) = %Trim(AuthKey));
          AuthKeyOk = *On;
        EndIf;
        Leave;
      EndIf;
    EndIf;
  EndDo;

  // Close the config file
  fclose(File_io);

  // If user, password and client are checked with success
  If (AuthKeyOk = *On);
    Return *On;
  EndIf;

  Return *Off;
End-Proc;

Dcl-Proc Header;
  Dcl-Pi Header;
    Status Char(3) Const;
  End-Pi;

  HttpResponse('Status: ' + Status + CHAR_CR + CHAR_LF);
  HttpResponse('X-Powered-By: IBMi' + CHAR_CR + CHAR_LF);
  HttpResponse('Content-Type: text/plain' + CHAR_CR + CHAR_LF + CHAR_CR + CHAR_LF);
End-Proc;

Dcl-Proc Cmd001;
  Dcl-S AspUsedCurrent Packed(5: 2);
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-Ds SSTS0200 Len(149) Qualified;
    AspUsed Int(10) Pos(53); 
  End-Ds;

  qwcrssts(SSTS0200: %Size(SSTS0200): 'SSTS0200': '*NO': Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    
  
  AspUsedCurrent = SSTS0200.AspUsed / 10000;

  HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);
  HttpResponse('Info=|AspUsed=' + %Trim(%Char(%Int(AspUsedCurrent))) + '|' + CHAR_CR + CHAR_LF);
End-Proc;

Dcl-Proc Cmd002;
  Dcl-S ParamOutQName Char(10);
  Dcl-S ParamOutQLib Char(10);
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-Ds OUTQ0100 Len(1200) Qualified;
    NumberOfFiles Int(10) Pos(93);
  End-Ds;

  ParamOutQName = HttpRequestValue(RequestData: 'OUTQNAME');
  If (ParamOutQName = *Blanks);
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Missing parameter OUTQNAME' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  ParamOutQLib = HttpRequestValue(RequestData: 'OUTQLIB');
  If (ParamOutQLib = *Blanks);
    ParamOutQLib = '*LIBL';
  EndIf;

  qsproutq(OUTQ0100: %Size(OUTQ0100): 'OUTQ0100': ParamOutQName + ParamOutQLib: Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    

  HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);
  HttpResponse('Info=' +
    '|NumberOfFiles=' + %Char(OUTQ0100.NumberOfFiles) +
    '|' + CHAR_CR + CHAR_LF);
End-Proc;

Dcl-Proc Cmd003;
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S UserSpaceName Char(20) Inz('JOBLIST   QTEMP');
  Dcl-Ds Fields Len(4) Qualified;
    List Int(10) Dim(1);
  End-Ds;
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds JOBL0100 Based(UserSpaceEntryPointer) Qualified;
    JobName Char(10) Pos(1);
    JobUser Char(10) Pos(11);
    JobNumber Char(6) Pos(21);
    InternalJobId Char(16) Pos(27);
    Status Char(10) Pos(43);
    Type Char(1) Pos(53);
    SubType Char(1) Pos(54);
  End-Ds;
  Dcl-Ds JOBI0200 Len(225) Qualified;
    SubsystemName Char(10) Pos(63);
    ActiveJobStatus Char(4) Pos(108);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-S ParamStatus Char(10);
  Dcl-S ParamJobName Char(10);

  ParamStatus = HttpRequestValue(RequestData: 'STATUS');
  If (ParamStatus = *Blanks);
    ParamStatus = '*ALL';
  EndIf;

  ParamJobName = HttpRequestValue(RequestData: 'JOBNAME');
  If (ParamJobName = *Blanks);
    ParamJobName = '*ALL';
  EndIf;

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    

  // Retrieve list of jobs
  Fields.List(1) = 101;
  qusljob(UserSpaceName: 'JOBL0100': ParamJobName + '*ALL      *ALL  ': ParamStatus: Error: '*': 1: Fields);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    

  // Retrieve the user space pointer
  qusptrus(UserSpaceName:UserSpaceDataPointer:Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);

  // Init the entry pointer
  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
  For i = 1 To UserSpaceHeader.NumberOfEntries;
    qusrjobi(JOBI0200: %Size(JOBI0200): 'JOBI0200': '*INT': JOBL0100.InternalJobId: Error);
    HttpResponse('Info=' +
      '|JobName=' + %Trim(JOBL0100.JobName) +
      '|JobUser=' + %Trim(JOBL0100.JobUser) +
      '|JobNumber=' + %Trim(JOBL0100.JobNumber) +
      '|Status=' + %Trim(JOBL0100.Status) +
      '|Type=' + %Trim(JOBL0100.Type) +
      '|SubType=' + %Trim(JOBL0100.SubType) +
      '|ActiveJobStatus=' + %Trim(JOBI0200.ActiveJobStatus) +
      '|SubsystemName=' + %Trim(JOBI0200.SubsystemName) +
      '|' + CHAR_CR + CHAR_LF);
    // Shift the entry pointer
    UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
  EndFor;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;

Dcl-Proc Cmd004;
  Dcl-S ParamSql Char(2000);
  Dcl-S String Char(32);
  Dcl-S StringNull Int(3);
  Dcl-s Step Int(10) Inz(1);

  ParamSql = HttpRequestValue(RequestData: 'SQL');
  If (ParamSql = *Blanks);
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Missing parameter SQL' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  If (Upper(%Subst(PAramSql: 1: 7)) <> 'SELECT ');
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Only SELECT is allowed' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  DoW (Step <= 5);
    Select;
      When (Step = 1);
        Exec Sql PREPARE S1 FROM :ParamSql;
      When (Step = 2);
        Exec Sql DECLARE C1 CURSOR FOR S1;
      When (Step = 3);
        Exec Sql OPEN C1;
      When (Step = 4);
        Exec Sql FETCH C1 INTO :String:StringNull;
      When (Step = 5);
        Exec Sql CLOSE C1;
    EndSl;
    If (SQLSTATE <> '00000');
      HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
      HttpResponse('Info=SQLSTATE:' + SQLSTATE + CHAR_CR + CHAR_LF);
      Leave;
    EndIf;
    Step += 1;
  EndDo; 

  If (StringNull = -1) ;
    String = '<NULL>';
  EndIf;

  If (SQLSTATE = '00000');
    HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=' + '|Value=' + %Trim(String) + '|' + CHAR_CR + CHAR_LF);
  EndIf;
End-Proc;

Dcl-Proc Cmd005;
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S UserSpaceName Char(20) Inz('JOBLIST   QTEMP');
  Dcl-S UserSpaceDataPointer Pointer;
  Dcl-Ds UserSpaceHeader LikeDs(UserSpaceHeader_Ds) Based(UserSpaceDataPointer);
  Dcl-S UserSpaceEntryPointer Pointer;
  Dcl-Ds SBSL0100 Based(UserSpaceEntryPointer) Qualified;
    SubsystemName Char(10) Pos(1);
    SubsystemLibraryName Char(10) Pos(11);
  End-Ds;
  Dcl-Ds SBSI0100 Len(80) Qualified;
    Status Char(10) Pos(29);
    CurrentlyActiveJobs Int(10) Pos(73);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-S ParamSbsName Char(10);

  ParamSbsName = HttpRequestValue(RequestData: 'SBSNAME');
  If (ParamSbsName = *Blanks);
    ParamSbsName = '*ALL';
  EndIf;

  // Create user space
  CreateUserSpace(UserSpaceName: Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    

  // Retrieve list of active subsystems
  qwclasbs(UserSpaceName: 'SBSL0100': Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;    

  // Retrieve the user space pointer
  qusptrus(UserSpaceName:UserSpaceDataPointer:Error);
  If (Error.BytesAvailable > 0); // Check error
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Api error' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);

  // Init the entry pointer
  UserSpaceEntryPointer = UserSpaceDataPointer + UserSpaceHeader.OffsetListData;
  For i = 1 To UserSpaceHeader.NumberOfEntries;
    If ((ParamSbsName = '*ALL') Or (SBSL0100.SubsystemName = ParamSbsName));
      qwdrsbsd(SBSI0100: %Size(SBSI0100): 'SBSI0100': SBSL0100.SubsystemName + SBSL0100.SubsystemLibraryName: Error);
      HttpResponse('Info=' +
        '|SubsystemName=' + %Trim(SBSL0100.SubsystemName) +
        '|SubsystemLibraryName=' + %Trim(SBSL0100.SubsystemLibraryName) +
        '|Status=' + %Trim(SBSI0100.Status) +
        '|CurrentlyActiveJobs=' + %Char(SBSI0100.CurrentlyActiveJobs) +
        '|' + CHAR_CR + CHAR_LF);
    EndIf;
    // Shift the entry pointer
    UserSpaceEntryPointer += UserSpaceHeader.SizeOfEntry;
  EndFor;

  // Delete user space
  DeleteUserSpace(UserSpaceName);

End-Proc;

Dcl-Proc Cmd006;
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-S ParamMsgQName Char(10);
  Dcl-S ParamMsgId Char(7);
  Dcl-S ParamMinutes Char(10);
  Dcl-Ds Receiver Len(8000) End-Ds;
  Dcl-S DataPointer1 Pointer;
  Dcl-Ds HSTL0100 Len(8000) Qualified Based(DataPointer1);
    Length Int(10) Pos(1);
    Severity Int(10) Pos(5);
    MessageId Char(7) Pos(9);
    MessageType Char(2) Pos(16);
    DateSent Char(7) Pos(38);
    TimeSent Char(6) Pos(45);
  End-Ds;
  Dcl-S DataPointer2 Pointer;
  Dcl-Ds LSTM0100 Len(8000) Qualified Based(DataPointer2);
    OffsetNextEntry Int(10) Pos(1);
    Severity Int(10) Pos(13);
    MessageId Char(7) Pos(17);
    MessageType Char(2) Pos(24);
    DateSent Char(7) Pos(70);
    TimeSent Char(6) Pos(77);
  End-Ds;
  Dcl-Ds ListInformation LikeDs(ListInformation_Ds);
  Dcl-Ds SelectionInformation1 Qualified;
    *N Int(10) Pos(1) Inz(92);
    StartDate Char(10) Pos(5) Inz('*CURRENT');
    StartTime Char(10) Pos(15) Inz('*AVAIL');
    StartMicroSeconds Char(6) Pos(25) Inz(*all'0');
    EndDate Char(10) Pos(31) Inz('*CURRENT');
    EndTime Char(10) Pos(41) Inz('*AVAIL');
    EndTimeMicroSeconds Char(6) Pos(51) Inz(*all'0');
    *N Int(10) Pos(57) Inz(1);
    *N Int(10) Pos(61) Inz(0);
    *N Int(10) Pos(65) Inz(0);
    *N Int(10) Pos(69) Inz(0);
    *N Int(10) Pos(73) Inz(0);
    *N Int(10) Pos(77) Inz(0);
    *N Int(10) Pos(81) Inz(1);
    *N Int(10) Pos(85) Inz(0);
    *N Int(10) Pos(89) Inz(0);
  End-Ds;
  Dcl-Ds SelectionInformation2 Qualified;
    *N Char(10) Pos(1) Inz('*PRV');
    *N Char(2) Pos(11) Inz(*Blanks);
    *N Int(10) Pos(13) Inz(0);
    *N Int(10) Pos(17) Inz(200);
    *N Int(10) Pos(21) Inz(200);
    *N Int(10) Pos(25) Inz(44);
    *N Int(10) Pos(29) Inz(1);
    *N Int(10) Pos(33) Inz(54);
    *N Int(10) Pos(37) Inz(58);
    *N Int(10) Pos(41) Inz(1); 
    *N Char(10) Pos(45) Dim(1) Inz('*ALL      ');
    *N Char(4) Pos(55) Dim(1) Inz(x'FFFFFFFF');
    *N Int(10) Pos(59) Dim(1) Inz(1001);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-S j Int(10);
  Dcl-S From Char(20);
  Dcl-Ds MsgQUsed Len(44) End-Ds;

  ParamMsgQName = HttpRequestValue(RequestData: 'MSGQNAME');
  If (ParamMsgQName = *Blanks);
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Missing parameter MSGQNAME' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  ParamMinutes = HttpRequestValue(RequestData: 'MINUTES');
  If (ParamMinutes = *Blanks);
    HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
    HttpResponse('Info=Missing parameter MINUTES' + CHAR_CR + CHAR_LF);
    Return;
  EndIf;

  ParamMsgId = HttpRequestValue(RequestData: 'MSGID');
  If (ParamMsgId = *Blanks);
    ParamMsgId = '*ALL';
  EndIf;

  From = %Char(%Timestamp() - %Minutes(%Int(ParamMinutes)): *iso0);

  If (ParamMsgQName = 'QHST');
    SelectionInformation1.StartDate = '1' + %Subst(From: 3: 6);
    SelectionInformation1.StartTime = %Subst(From: 9: 6);
    // Open the list
    qmholhst(Receiver: %Size(Receiver): 'HSTL0100': ListInformation: -1: SelectionInformation1: 65535: '*SYS': Error);
    If (Error.BytesAvailable > 0); // Check error
      HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
      HttpResponse('Info=Api error ' + Error.ExceptionId + CHAR_CR + CHAR_LF);
      Return;
    Else; 
      HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);
      // Read all messages from the list
      j = 0;
      DoW (j < ListInformation.TotalRecords);
        DataPointer1 = %Addr(Receiver);
        For i=1 To ListInformation.RecordsReturned;
          If ((ParamMsgId = '*ALL') Or (HSTL0100.MessageId = ParamMsgId));
            HttpResponse('Info=|MessageId=' + %Trim(HSTL0100.MessageId) + 
              '|Date=' + %Subst(HSTL0100.DateSent: 2: 6) +
              '|Time=' + HSTL0100.TimeSent +
              '|Severity=' + %Char(HSTL0100.Severity) +
              '|Type=' + HSTL0100.MessageType +
              '|' + CHAR_CR + CHAR_LF);
            EndIf;
          DataPointer1 += HSTL0100.Length;
          j += 1;
        EndFor;
        If (j < ListInformation.TotalRecords);
          qgygtle(Receiver: %Size(Receiver): ListInformation.Handle: ListInformation: (ListInformation.TotalRecords-j): j+1: Error);
          If (Error.BytesAvailable > 0); // Check error
            HttpResponse('Info=Api error ' + Error.ExceptionId + CHAR_CR + CHAR_LF);
            Leave;
          EndIf;
        EndIf;
      EndDo;
      // Close the list
      qgyclst(ListInformation.Handle: Error);
    EndIf;
  Else;
    qgyolmsg(Receiver: %Size(Receiver): ListInformation: -1: '0': SelectionInformation2: %Size(SelectionInformation2): '1' + ParamMsgQName + '*LIBL': MsgQUsed: Error);
    If (Error.BytesAvailable > 0); // Check error
      HttpResponse('Result=NOK' + CHAR_CR + CHAR_LF);
      HttpResponse('Info=Api error ' + Error.ExceptionId + CHAR_CR + CHAR_LF);
      Return;
    Else;
      HttpResponse('Result=OK' + CHAR_CR + CHAR_LF);
      // Read all messages from the list
      j = 0;
      DoW (j < ListInformation.TotalRecords);
        DataPointer2 = %Addr(Receiver);
        For i=1 To ListInformation.RecordsReturned;
          If (((ParamMsgId = '*ALL') Or (LSTM0100.MessageId = ParamMsgId)) And (%Subst(LSTM0100: 71: 12) >= %Subst(From: 3: 12)));
            HttpResponse('Info=|MessageId=' + %Trim(LSTM0100.MessageId) +
              '|Date=' + %Subst(LSTM0100.DateSent: 2: 6) +
              '|Time=' + LSTM0100.TimeSent +
              '|Severity=' + %Char(LSTM0100.Severity) +
              '|Type=' + LSTM0100.MessageType +
              '|' + CHAR_CR + CHAR_LF);
          EndIf;
          DataPointer2 = %Addr(Receiver) + LSTM0100.OffsetNextEntry;
          j += 1;
        EndFor;
        If (j < ListInformation.TotalRecords);
          qgygtle(Receiver: %Size(Receiver): ListInformation.Handle: ListInformation: (ListInformation.TotalRecords-j): j+1: Error);
          If (Error.BytesAvailable > 0); // Check error
            HttpResponse('Info=Api error ' + Error.ExceptionId + CHAR_CR + CHAR_LF);
            Leave;
          EndIf;
        EndIf;
      EndDo;
      // Close the list
      qgyclst(ListInformation.Handle: Error);
    EndIf; 
  EndIf;

End-Proc;
