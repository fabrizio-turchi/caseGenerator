program caseGenerator;

uses
  System.StartUpCopy,
  FMX.Forms,
  caseGenerator_main in 'caseGenerator_main.pas' {formMain},
  caseGenerator_GeneralData in 'caseGenerator_GeneralData.pas' {formNewCase},
  caseGenerator_identity in 'caseGenerator_identity.pas' {formIdentity},
  caseGenerator_location in 'caseGenerator_location.pas' {formLocation},
  caseGenerator_role in 'caseGenerator_role.pas' {formRole},
  caseGenerator_tool in 'caseGenerator_tool.pas' {formTool},
  caseGenerator_trace_SIM in 'caseGenerator_trace_SIM.pas' {formTraceSIM},
  caseGenerator_trace_mobile in 'caseGenerator_trace_mobile.pas' {formTraceMobile},
  caseGenerator_trace_computer in 'caseGenerator_trace_computer.pas' {formTraceComputer},
  caseGenerator_relationship in 'caseGenerator_relationship.pas' {formRelationship},
  caseGenerator_investigative_action in 'caseGenerator_investigative_action.pas' {formInvestigativeAction},
  caseGenerator_trace_file in 'caseGenerator_trace_file.pas' {formTraceFile},
  caseGenerator_provenance_record in 'caseGenerator_provenance_record.pas' {formProvenanceRecord},
  caseGenerator_trace_disk_partition in 'caseGenerator_trace_disk_partition.pas' {formTraceDiskPartition},
  caseGenerator_trace_mobile_account in 'caseGenerator_trace_mobile_account.pas' {formTraceMobileAccount},
  caseGenerator_trace_message in 'caseGenerator_trace_message.pas' {formTraceMessage},
  caseGenerator_warrant in 'caseGenerator_warrant.pas' {formWarrant},
  caseGenerator_util in 'caseGenerator_util.pas',
  caseGenerator_overview in 'caseGenerator_overview.pas' {formOverview},
  caseGenerator_trace_email_account in 'caseGenerator_trace_email_account.pas' {formTraceEmailAccount},
  caseGenerator_trace_phone_account in 'caseGenerator_trace_phone_account.pas' {formTracePhoneAccount};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.CreateForm(TformNewCase, formNewCase);
  Application.CreateForm(TformIdentity, formIdentity);
  Application.CreateForm(TformLocation, formLocation);
  Application.CreateForm(TformRole, formRole);
  Application.CreateForm(TformTool, formTool);
  Application.CreateForm(TformTraceSIM, formTraceSIM);
  Application.CreateForm(TformTraceMobile, formTraceMobile);
  Application.CreateForm(TformTraceComputer, formTraceComputer);
  Application.CreateForm(TformRelationship, formRelationship);
  Application.CreateForm(TformInvestigativeAction, formInvestigativeAction);
  Application.CreateForm(TformTraceFile, formTraceFile);
  Application.CreateForm(TformProvenanceRecord, formProvenanceRecord);
  Application.CreateForm(TformTraceDiskPartition, formTraceDiskPartition);
  Application.CreateForm(TformTraceMobileAccount, formTraceMobileAccount);
  Application.CreateForm(TformTraceMessage, formTraceMessage);
  Application.CreateForm(TformWarrant, formWarrant);
  Application.CreateForm(TformOverview, formOverview);
  Application.CreateForm(TformTraceEmailAccount, formTraceEmailAccount);
  Application.CreateForm(TformTracePhoneAccount, formTracePhoneAccount);
  Application.Run;
end.
