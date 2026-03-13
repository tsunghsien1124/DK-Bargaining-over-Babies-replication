%
% Figure 7: Relative cost of real-life policies raising the total fertility rate by 0.1
%

clear; clc;

colortax = 0.90*[1 1 1];
colorcc  = 0.15*[1 1 1];
colorpl  = 0.65*[1 1 1];

% read input data
fileID = fopen('../output/res_policy.out','r');
A = textscan(fileID, '%s %s %s %s %s %s');
fclose(fileID);


%% TOTAL COST PER COUPLE

x = [str2double(A{4}{ 3}) str2double(A{4}{ 4}) str2double(A{4}{ 5});
     str2double(A{4}{11}) str2double(A{4}{12}) str2double(A{4}{13});
     str2double(A{4}{19}) str2double(A{4}{20}) str2double(A{4}{21}) ];

width = 0.25;

set(0,'defaulttextfontsize',18);

figure(103);
hold on; box off;
bar([1-width 2-width 3-width], x(:, 1), width,'FaceColor', colortax)
bar([1       2       3      ], x(:, 2), width,'FaceColor', colorcc)
bar([1+width 2+width 3+width], x(:, 3), width,'FaceColor', colorpl)
set(gca, 'XTick', [1 2 3]);
set(gca, 'XTickLabels', {'all children', 'from 2nd child', 'from 3rd child'});
ylim([0 1.1])
xlim([0.5 3.5])
legend('Tax Credit', 'Child Care', 'Parental Leave', 'Location', 'NorthEast');
title('Total Cost per Couple')
hold off;
