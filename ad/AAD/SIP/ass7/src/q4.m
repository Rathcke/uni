clear all;

I = imread('rice.png');

figure
subplot(2,3,1)
imshow(I); title('Original image')
place = 2;

for i=2:1:6
    se = strel('disk', i);
    I2 = imtophat(I, se);
    I2 = imadjust(I2);
    I3 = imabsdiff(I, I2);
    
    subplot(2,3,place)
    imshow(I2); 
    title(strcat('Disk size: ',num2str(i)))

    place = place + 1;
end

figure
place = 1;
for i=24:2:36
    se = strel('disk', i);
    I2 = imtophat(I, se);
    I2 = imadjust(I2);
    I3 = imabsdiff(I, I2);
    
    subplot(2,3,place)
    imshow(I2); 
    title(strcat('Disk size: ',num2str(i)))

    place = place + 1;
end
%%
clear all;

I = imread('rice.png');
se = strel('disk', 10);
I2 = imtophat(I, se);
I2 = imadjust(I2);

I3 = imabsdiff(I, I2);

imshow(I3)