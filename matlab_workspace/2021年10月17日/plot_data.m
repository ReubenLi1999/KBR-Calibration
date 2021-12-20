clc

figure('color', [1 1 1], 'units', 'normalized', 'outerposition', [0 0 1 1]);
plot(1: 5: 1000, 0.0002 * sin(2*pi / 251 * [1: 5: 1000] + 20)' + 0.00005 * sin(2*pi / 125.5 * [1: 5: 1000] + 20)', 'red', 'linewidth', 2.70); hold on
grid on;
set(gcf, 'Units', 'centimeters', 'Position', [10 10 70 60]);
xlabel('Time [s]', 'fontsize', 20);
ylabel('Amplitude [m]', 'fontsize', 20);
set(gca, 'FontSize', 20, 'linewidth', 1.1)
% legend('filtered', 'simulated')
% plot(1: 5: 1000, TH0401ROI1B20190101A0420210802000000(1: 5: 1000, 1) - trajectory20210908200940(1: 200, 1), 'linewidth', 2.7); hold on
% plot(1: 5: 1000, TH0401ROI1B20190101A0420210802000000(1: 5: 1000, 2) - trajectory20210908200940(1: 200, 2), 'linewidth', 2.7); hold on
% plot(1: 5: 1000, TH0401ROI1B20190101A0420210802000000(1: 5: 1000, 3) - trajectory20210908200940(1: 200, 3), 'linewidth', 2.7)
% grid on;
% legend('x-leading', 'y-leading', 'z-leading')
% set(gcf, 'Units', 'centimeters', 'Position', [10 10 70 60]);
% xlabel('Time [s]', 'fontsize', 20);
% ylabel('Residual [m]', 'fontsize', 20);
% set(gca, 'FontSize', 20, 'linewidth', 1.1)
% plot(1: 5: 4000, trajectory20210831202943(:, 1), 'linewidth', 2.7); hold on
% plot(1: 5: 4000, trajectory20210831202943(:, 2), 'linewidth', 2.7); hold on
% plot(1: 5: 4000, trajectory20210831202943(:, 3), 'linewidth', 2.7); hold on
% plot(1: 5: 4000, trajectory20210831202943(:, 4), 'linewidth', 2.7); hold on
% plot(1: 5: 4000, trajectory20210831202943(:, 5), 'linewidth', 2.7); hold on
% plot(1: 5: 4000, trajectory20210831202943(:, 6), 'linewidth', 2.7); 
% grid on;
% legend('x-leading', 'y-leading', 'z-leading', 'x-tracking', 'y-tracking', 'z-tracking')
% set(gcf, 'Units', 'centimeters', 'Position', [10 10 70 60]);
% xlabel('Time [s]', 'fontsize', 20);
% ylabel('Amplitude [m]', 'fontsize', 20);
% set(gca, 'FontSize', 20, 'linewidth', 1.1)
% Y = fft(antphasecentrecorr2degree(201: 400));P2=abs(Y/200);P1=P2(1: 101); P1(2:end-1) = 2 * P1(2:end-1);f = 0.2*(0:100)/200;
% figure('color', [1 1 1])
% loglog(f, P1, 'linewidth', 2.7); grid on;
% set(gcf, 'Units', 'centimeters', 'Position', [10 10 70 60]);
% xlabel('Frequency [Hz]', 'fontsize', 20);
% ylabel('Amplitude [m]', 'fontsize', 20);
% set(gca, 'FontSize', 20, 'linewidth', 1.1)