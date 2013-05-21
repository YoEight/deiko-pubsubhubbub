create table subscriptions (
  topic         varchar not null,
  callback      varchar not null,
  lease_seconds number  not null,
  secret        varchar
);

create table fetched_feeds (
  feed_id    varchar not null,
  fetch_date number  not null,
  xml        blob    not null
);
