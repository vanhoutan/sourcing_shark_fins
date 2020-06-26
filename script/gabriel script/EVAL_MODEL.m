%% base
clear all
close all
%% chargement de base
load META_SFISH
% A=SFISH(:,1:2);
% load LOBSTER_TO_MODEL
% A=[A;LOBSTER(:,1:2)];
% load KRILL_TO_MODEL
% A=[A;KRILL(:,1:2)];
% load CORAL_TO_MODEL
% A=[A;CORAL(:,1:2)];
% load CEPHALOPOD_TO_MODEL
% A=[A;CEPHALOPOD(:,1:2)];
% load REPT_TO_MODEL
% A=[A;REPT(:,1:2)];
% load META_MAMMALS
% A=[A;REPT(:,1:2)];
% SFISH=A;
% load COO_NEREUS
% SP_TO_MODEL_MAMMALS=A;
addpath(genpath('C:\Users\Gabriel\Desktop\IPBES_MODEL\IPBES_MODEL'))
%% Sharck only 
load  ALL_GRP_CLASSIFICATION SHARK
TST=ones(length(SHARK),1)*0;

for i=1:length(SHARK)
    x=find(SFISH(:,1)==SHARK(i,1));
    if ~isempty(x)
    TST(i)=SFISH(x(1),2);
    end
end

FISH=[SHARK TST];
x=find(TST==0);
FISH(x,:)=[];
SFISH=FISH;
%% all together 

%%
% B=ones(length(COO),1)*0;
% for i=1:length(SFISH)
%         load(['IPBES_MODEL_MERGE_SP_',num2str(SFISH(i,2))],'MODEL')
%         y=find(MODEL>0);
%         tresh=prctile(MODEL(y),5);
%         x=find(MODEL>tresh);
%         B(x)=B(x)+1;
% end

%% on exporte pour tyler 
addpath('C:\Users\Gabriel\Desktop\IPBES_MODEL\TRESHOLD\FISH\THESHOLD')
addpath('C:\Users\Gabriel\Desktop\IPBES_MODEL\species')
load  COO_NEREUS
load META_IPBES NAME
for i=186:300
        load(['IPBES_MODEL_MERGE_SP_',num2str(SFISH(i,2))],'MODEL')
        load (['SP_IPBES_',num2str(SFISH(i,2))],'PSC')
        t=table(COO(:,1),COO(:,2),PSC,MODEL,'VariableNames',{'Lon','Lat','OBS','MODELAVG'});
        N=NAME{SFISH(i,2),1};
        writetable(t,[N,'_OBS_MODEL.csv'],'delimiter',',','WriteRowNames',false)  
end