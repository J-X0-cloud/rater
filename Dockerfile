# Build stage: restore and publish the API with the SDK image.
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src

COPY global.json Directory.Build.props Rater.sln ./
COPY src/Rater.Core/Rater.Core.fsproj src/Rater.Core/
COPY src/Rater.Api/Rater.Api.fsproj src/Rater.Api/
RUN dotnet restore src/Rater.Api/Rater.Api.fsproj

COPY src/ src/
RUN dotnet publish src/Rater.Api/Rater.Api.fsproj -c Release -o /app --no-restore

# Runtime stage: ASP.NET runtime only, running as the image's non-root user.
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS runtime
WORKDIR /app

COPY --from=build /app ./
COPY products/ ./products/

ENV PORT=8080 \
    RATER_PRODUCTS_DIR=/app/products \
    DOTNET_gcServer=0

USER $APP_UID
EXPOSE 8080

# The API binds to 0.0.0.0:$PORT, so platforms that inject PORT are honoured.
ENTRYPOINT ["dotnet", "rater-api.dll"]
