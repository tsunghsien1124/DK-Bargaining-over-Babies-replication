%
% Figure 6: Relative cost of targeted subsidies needed to raise the total fertility rate by 0.1
%

clear; clc;

% set color
colorwomen = 0.15*[1 1 1];
colormen   = 0.70*[1 1 1];
    
% read input data
fileID = fopen('../output/res_policy.out','r');
A = textscan(fileID, '%s %s %s %s %s %s');
fclose(fileID);


%% SUBSIDY PER CHILD PER YEAR

x = [str2double(A{3}{ 7}) str2double(A{3}{ 8});
     str2double(A{3}{15}) str2double(A{3}{16});
     str2double(A{3}{23}) str2double(A{3}{24})];

width = 0.4;

set(0,'defaulttextfontsize',18);

figure(101);
hold on; box off;
bar([1-width/2 2-width/2 3-width/2], x(:, 1), width,'FaceColor', colorwomen)
bar([1+width/2 2+width/2 3+width/2], x(:, 2), width, 'FaceColor', colormen)
set(gca, 'XTick', [1 2 3]);
set(gca, 'XTickLabels', {'all children', 'from 2nd child', 'from 3rd child'});
ylim([0 8])
xlim([0.5 3.5])
legend('to women', 'to men', 'Location', 'NorthWest');
title('Subsidy per Child per Year')
hold off


%% TOTAL COST PER COUPLE

x = [str2double(A{4}{ 7}) str2double(A{4}{ 8});
     str2double(A{4}{15}) str2double(A{4}{16});
     str2double(A{4}{23}) str2double(A{4}{24})];

figure(102);
hold on; box off;
bar([1-width/2 2-width/2 3-width/2], x(:, 1), width,'FaceColor', colorwomen)
bar([1+width/2 2+width/2 3+width/2], x(:, 2), width, 'FaceColor', colormen)
set(gca, 'XTick', [1 2 3]);
set(gca, 'XTickLabels', {'all children', 'from 2nd child', 'from 3rd child'});
ylim([0 3])
xlim([0.5 3.5])
legend('to women', 'to men', 'Location', 'NorthEast');
title('Total Cost per Couple')
hold off
