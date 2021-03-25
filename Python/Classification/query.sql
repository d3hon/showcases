WITH range_of_dates AS ( -- to get mentioned above range of dates from transactions table
SELECT 
    date_trunc('month', created_date) as month_start
FROM transactions
WHERE created_date between 'Sep 1, 2018' and 'Apr 1, 2019'
GROUP BY 1
)

SELECT 

    month_start,
    t1.user_id,
    case 
        when t4.user_id is not null then 1 
        else 0 
    end is_engaged_next_period, -- target variable
    case 
        when extract(year from month_start) - "birth_year" <= 25 then '18-25'
        when extract(year from month_start) - "birth_year" between 26 and 30 then '26-30'
        when extract(year from month_start) - "birth_year" between 31 and 40 then '31-40'
        when extract(year from month_start) - "birth_year" > 40 then '40 plus'
    end as age,
    country,
    city,
    datediff('month', t1.created_date, month_start) as tenure,
    user_settings_crypto_unlocked,
    case -- in original dataset there are 4 plans, for sake of simplicity let's consider just two
        when plan = 'STANDARD' then 0
        else 1
    end as is_premium_plan,
    attributes_notifications_marketing_push,
    attributes_notifications_marketing_email,
    num_contacts,
    num_referrals,
    num_successful_referrals,
    brand,
    case 
        when t6.user_id is not null then 1
        else 0
    end as has_received_notification,
    coalesce(count(distinct date_trunc('day', t3.created_date)),0) as number_orders, -- number of days with transactions
    extract(days from month_start -  max(date_trunc('day', t3.created_date)) ) as days_since_last_transaction,
    (extract(days from max(date_trunc('day', t3.created_date))-  min(date_trunc('day', t3.created_date)) ) + 1) as active_period,
    (extract(days from max(date_trunc('day', t3.created_date))-  min(date_trunc('day', t3.created_date)) ) + 1)/count(distinct date_trunc('day', t3.created_date)) as frequency
    
FROM users t1

JOIN range_of_dates t2
ON month_start > t1.created_date -- to get only those users who created account before given date

JOIN devices t5
ON t5.user_id = t1.user_id

LEFT JOIN transactions t3
ON month_start > t3.created_date -- to capture only transactions that happened before given date
AND t3.user_id = t1.user_id
AND t3.transactions_state = 'COMPLETED'
AND t3.transactions_type IN ('EXCHANGE', 'CARD_PAYMENT', 'TRANSFER', 'ATM')

LEFT JOIN transactions t4 
ON t4.created_date >= month_start and extract(days from date_trunc('day', t4.created_date) - month_start) < 31
-- to capture transactions that happened within 30 days after considered date
AND t4.transactions_state = 'COMPLETED'
AND t4.transactions_type IN ('EXCHANGE', 'CARD_PAYMENT', 'TRANSFER', 'ATM')
AND t4.user_id = t1.user_id

LEFT JOIN notifications t6
ON t1.user_id = t6.user_id 
AND status = 'SENT' 
AND extract(days from  month_start - date_trunc('day', t6.created_date)) between 1 and 7 
-- to check if user received any notifications within 7 days prior to given date
 
GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16