% These codes were used to create a tree diagram to illustrate full carbon
% budget

T = readtable('all_GPP_together_per_SITE_20221122.csv');
%for ii = 1:size(T,1)
ii = 1;
% the for loop will go through every row of all_GPP_together_per_SITE_20220322
% add annotation (which is data)

figure1 = figure;
figure1.Position = [1 1 3742 2268];
% We first create the figure with correct dimensions 

I = imread('nice_background_template_V3_huanyuan.jpg');
h = image(xlim, ylim, I);
uistack(h, 'bottom')
% add the background tree
axis off
hold on
my_string0 = sprintf('Site: %s',T{ii,1}{1,1});
annotation(figure1,'textbox',...
    [0.251717909608056 0.887289927764358 0.146476196558931 0.0169027795120226],...
    'String',my_string0,...
    'FontWeight','bold',...
    'FontSize',36,...
    'FontName','Calibri',...
    'FitBoxToText','off',...
    'EdgeColor','none');
my_string1 = sprintf('R_{leaf} = %.2f ± %.2f',T{ii,"R_leaf"} , T{ii,"R_leaf_se"})
annotation('textbox',[0.318089233958184 0.630345863925823 0.0709068234455728 0.0267942583732057],'String',my_string1,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string2 = sprintf('GPP = %.2f ± %.2f',T{ii,"GPP"} , T{ii,"GPP_se"})
annotation('textbox',[0.470744438288868 0.864634632733673 0.0716194548369856 0.0207336523125997],'String',my_string2,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string3 = sprintf('NPP_{leaf} = %.2f ± %.2f',T{ii,"NPP_leaf"} , T{ii,"NPP_leaf_se"})
annotation('textbox',[0.47849180289788 0.686296817669907 0.0766078745768751 0.0267942583732057],'String',my_string3,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string4 = sprintf('NPP_{herbivory} = %.2f ± %.2f',T{ii,"NPP_herbivory"} , T{ii,"NPP_herbivory_se"})
annotation('textbox',[0.475414926905726 0.660271565495536 0.0820416889363977 0.0245614035087719],'String',my_string4,'FontSize',18,'FitBoxToText','on','EdgeColor','none')
my_string5 = sprintf('NPP = %.2f ± %.2f',T{ii,"NPP"} , T{ii,"NPP_se"})
annotation('textbox',[0.558908033835691 0.862635204352826 0.071084981293426 0.0207336523125997],'String',my_string5,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string6 = sprintf('NPP_{AG} = %.2f ± %.2f',T{ii,"NPP_AG"} , T{ii,"NPP_AG_se"})
annotation('textbox',[0.559958901652274 0.755577524922594 0.0757170853376091 0.0267942583732057],'String',my_string6,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string7 = sprintf('NPP_{BG} = %.2f ± %.2f',T{ii,"NPP_BG"} , T{ii,"NPP_BG_se"})
annotation('textbox',[0.559846075339414 0.775202108127184 0.0757170853376091 0.0267942583732057],'String',my_string7,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string8 = sprintf('R_{autotrophic} = %.2f ± %.2f',T{ii,"R_autotrophic"} , T{ii,"R_autotrophic_se"})
annotation('textbox', [0.559646559988983 0.797947475178075 0.089969713165865 0.0267942583732057],'String',my_string8,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string9 = sprintf('NPP_{branch turnover} = %.2f ± %.2f',T{ii,"NPP_branch_turnover"} , T{ii,"NPP_branch_turnover_se"})
annotation('textbox',[0.49883808221537 0.582203545432741 0.107429182255478 0.0267942583732057],'String',my_string9,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string10 = sprintf('R_{stem} = %.2f ± %.2f',T{ii,"R_stem"} , T{ii,"R_stem_se"})
annotation('textbox', [0.408927045442937 0.525089054629695 0.0701941920541599 0.0267942583732057],'String',my_string10,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string11 = sprintf('D_{litter fall} = %.2f ± %.2f',T{ii,"D_litter_fall"} , T{ii,"D_litter_fall_se"})
annotation('textbox',[0.588857563293754 0.486565002015465 0.0774986638161411 0.0267942583732058],'String',my_string11,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string12 = sprintf('NPP_{all stem} = %.2f ± %.2f',T{ii,"NPP_all_stem"} , T{ii,"NPP_all_stem_se"})
annotation('textbox', [0.469727861465199 0.490694189006695 0.0871191876002138 0.0267942583732057],'String',my_string12,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string13 = sprintf('R_{soil heterotrophic} = %.2f ± %.2f',T{ii,"R_soil_heterotrophic"} , T{ii,"R_soil_heterotrophic_se"})
annotation('textbox',[0.27880114820657 0.224180822804951 0.105113130233387 0.0267942583732057],'String',my_string13,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
%my_string14 = sprintf('R_{coarse root} = %.2f ± %.2f',T{ii,"R_coarse_root"} , T{ii,"R_coarse_root_se"})
%annotation('textbox',[.2 .5 .3 .5],'String',my_string14,'FitBoxToText','on','EdgeColor','none')
my_string15 = sprintf('NPP_{coarse root} = %.2f ± %.2f',T{ii,"NPP_coarseroot"} , T{ii,"NPP_coarseroot_se"})
annotation('textbox',[0.468360190047432 0.215866623669526 0.0960270799928736 0.0267942583732057],'String',my_string15,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string16 = sprintf('NPP_{fine root} = %.2f ± %.2f',T{ii,"NPP_fineroot"} , T{ii,"NPP_fineroot_se"})
annotation('textbox', [0.468309633771741 0.240577462479037 0.0880099768394798 0.0267942583732057],'String',my_string16,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string17 = sprintf('CUE = %.2f ± %.2f',T{ii,"CUE"} , T{ii,"CUE_se"})
annotation('textbox',  [0.558614941429896 0.843622617303903 0.067521824336362 0.0207336523125997],'String',my_string17,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string18 = sprintf('D_{CWD} = %.2f ± %.2f',T{ii,"D_CWD"} , T{ii,"D_CWD_se"})
annotation('textbox', [0.539577552700345 0.354302004389121 0.0714412969891324 0.0267942583732058],'String',my_string18,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string19 = sprintf('D_{root} = %.2f ± %.2f',T{ii,"D_root"} , T{ii,"D_root_se"})
annotation('textbox', [0.575922020602186 0.248565135977846 0.0676999821842152 0.0267942583732057],'String',my_string19,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string20 = sprintf('R_{soil with coarseroot with litter} \n      %.2f ± %.2f',T{ii,"R_soil_with_coarseroot_with_litter"} , T{ii,"R_soil_with_coarseroot_with_litter_se"})
annotation('textbox', [0.302963520007907 0.394777181273482 0.0890789239265989 0.0430622009569378],'String',my_string20,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string21 = sprintf('R_{heterotrophic} = %.2f ± %.2f',T{ii,"R_heterotrophic"} , T{ii,"R_heterotrophic_se"})
annotation('textbox',  [0.559460475826019 0.818738720137475 0.0949581329057545 0.0267942583732057],'String',my_string21,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string22 = sprintf('R_{coarse root} = %.2f ± %.2f',T{ii,"R_coarse_root"} , T{ii,"R_coarse_root_se"})
annotation('textbox', [0.325632877305892 0.308497557344553 0.086406556208801 0.0267942583732058],'String',my_string22,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string23 = sprintf('R_{rhizosphere} = %.2f ± %.2f',T{ii,"R_rhizosphere"} , T{ii,"R_rhizosphere_se"})
annotation('textbox', [0.326018009691534 0.281848453224652 0.0876536611437734 0.0267942583732058],'String',my_string23,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string24 = sprintf('R_{cwd} = %.2f ± %.2f',T{ii,"R_cwd"} , T{ii,"R_cwd_se"})
annotation('textbox', [0.378415104516526 0.429021198716958 0.0678781400320684 0.0267942583732058],'String',my_string24,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
%my_string25 = sprintf('NEP = %.2f ± %.2f',T{ii,"NEP"} , T{ii,"NEP_se"})
%annotation('textbox', [0.745982361671009 0.679639252868877 0.16429839678916 0.041147131314896],'String',my_string25,'FontSize',20,'FitBoxToText','on','EdgeColor','none')
my_string26 = sprintf('NPP_{Herbs} = %.1f ± %.1f',T{ii,"NPP_herbs"} , T{ii,"NPP_herbs_se"})
annotation('textbox',[0.63856919621228 0.361292900153598 0.0746481382504899 0.0267942583732058],'String',my_string26,'FontSize',20,'FitBoxToText','on','EdgeColor','none')

saveas(figure1,sprintf('%s_whole_tree_carbon.png', T{ii,1}{1,1}))
hold off
%close(f)
%end